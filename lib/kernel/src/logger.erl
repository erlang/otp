%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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
-moduledoc """
API module for Logger, the standard logging facility in Erlang/OTP.

This module implements the main API for logging in Erlang/OTP. To create a log
event, use the [API functions](#logging-api-functions) or the log
[macros](#module-macros), for example:

```erlang
?LOG_ERROR("error happened because: ~p", [Reason]).   % With macro
logger:error("error happened because: ~p", [Reason]). % Without macro
```

To configure the Logger backend, use
[Kernel configuration parameters](kernel_app.md#logger) or
[configuration functions](#configuration-api-functions) in the Logger API.

By default, the Kernel application installs one log handler at system start.
This handler is named `default`. It receives and processes standard log events
produced by the Erlang runtime system, standard behaviours and different
Erlang/OTP applications. The log events are by default printed to the terminal.

If you want your systems logs to be printed to a file instead, you must
configure the default handler to do so. The simplest way is to include the
following in your [`sys.config`](config.md):

```erlang
[{kernel,
  [{logger,
    [{handler, default, logger_std_h,
      #{config => #{file => "path/to/file.log"}}}]}]}].
```

For more information about:

- the Logger facility in general, see the [User's Guide](logger_chapter.md).
- how to configure Logger, see the
  [Configuration](logger_chapter.md#configuration) section in the User's Guide.
- the built-in handlers, see `m:logger_std_h` and `m:logger_disk_log_h`.
- the built-in formatter, see `m:logger_formatter`.
- built-in filters, see `m:logger_filters`.

## Macros

The following macros are defined in `logger.hrl`, which is included in a module
with the directive

```erlang
    -include_lib("kernel/include/logger.hrl").
```

- `?LOG_EMERGENCY(StringOrReport[,Metadata])`
- `?LOG_EMERGENCY(FunOrFormat,Args[,Metadata])`
- `?LOG_ALERT(StringOrReport[,Metadata])`
- `?LOG_ALERT(FunOrFormat,Args[,Metadata])`
- `?LOG_CRITICAL(StringOrReport[,Metadata])`
- `?LOG_CRITICAL(FunOrFormat,Args[,Metadata])`
- `?LOG_ERROR(StringOrReport[,Metadata])`
- `?LOG_ERROR(FunOrFormat,Args[,Metadata])`
- `?LOG_WARNING(StringOrReport[,Metadata])`
- `?LOG_WARNING(FunOrFormat,Args[,Metadata])`
- `?LOG_NOTICE(StringOrReport[,Metadata])`
- `?LOG_NOTICE(FunOrFormat,Args[,Metadata])`
- `?LOG_INFO(StringOrReport[,Metadata])`
- `?LOG_INFO(FunOrFormat,Args[,Metadata])`
- `?LOG_DEBUG(StringOrReport[,Metadata])`
- `?LOG_DEBUG(FunOrFormat,Args[,Metadata])`
- `?LOG(Level,StringOrReport[,Metadata])`
- `?LOG(Level,FunOrFormat,Args[,Metadata])`

All macros expand to a call to Logger, where `Level` is taken from the macro
name, or from the first argument in the case of the `?LOG` macro. Location data
is added to the metadata as described under the `t:metadata/0` type definition.

The call is wrapped in a case statement and will be evaluated only if `Level` is
equal to or below the configured log level.

## See Also

[`config`](config.md), `m:erlang`, `m:io`, `m:logger_disk_log_h`,
`m:logger_filters`, `m:logger_handler`, `m:logger_formatter`, `m:logger_std_h`,
`m:unicode`
""".
-moduledoc(#{since => "OTP 21.0"}).

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
         add_handlers/1,
         reconfigure/0]).

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
-doc "A log event passed to filters and handlers".
-type log_event() :: #{level:=level(),
                       msg:={io:format(),[term()]} |
                            {report,report()} |
                            {string,unicode:chardata()},
                       meta:=metadata()}.
-doc "The severity level for the message to be logged.".
-type level() :: emergency | alert | critical | error |
                 warning | notice | info | debug.
-doc "A log report.".
-type report() :: map() | [{atom(),term()}, ...].
-doc """
A fun which converts a [`report()`](`t:report/0`) to a format string and
arguments, or directly to a string.

See section [Log Message](logger_chapter.md#log-message) in the User's Guide
for more information.
""".
-type report_cb() :: fun((report()) -> {io:format(),[term()]}) |
                     fun((report(),report_cb_config()) -> unicode:chardata()).
-doc "".
-type report_cb_config() :: #{depth       := pos_integer() | unlimited,
                              chars_limit := pos_integer() | unlimited,
                              single_line := boolean()}.
-doc "".
-type msg_fun() :: fun((term()) -> msg_fun_return() | {msg_fun_return(), metadata()}).
-doc "".
-type msg_fun_return() :: {io:format(),[term()]} |
                           report() |
                           unicode:chardata() |
                           ignore.
-doc """
Metadata for the log event.

Logger adds the following metadata to each log event:

- `pid => self()`
- `gl => group_leader()`
- `time => logger:timestamp()`

When a log macro is used, Logger also inserts location information:

- `mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}`
- `file => ?FILE`
- `line => ?LINE`

You can add custom metadata, either by:

- specifying a map as the last parameter to any of the log macros or the logger
  API functions.
- setting process metadata with `set_process_metadata/1` or
  `update_process_metadata/1`.
- setting primary metadata with `set_primary_config/1` or through the kernel
  configuration parameter [logger_metadata](kernel_app.md#logger_metadata)

> #### Note {: .info }
>
> When adding custom metadata, make sure not to use any of the keys mentioned
> above as that may cause a lot of confusion about the log events.

Logger merges all the metadata maps before forwarding the log event to the
handlers. If the same keys occur, values from the log call overwrite process
metadata, which overwrites the primary metadata, which in turn overwrite values
set by Logger.

The following custom metadata keys have special meaning:

- **`domain`** - The value associated with this key is used by filters for
  grouping log events originating from, for example, specific functional areas.
  See `logger_filters:domain/2` for a description of how this field can be used.

- **`report_cb`** - If the log message is specified as a `t:report/0`, the
  `report_cb` key can be associated with a fun (report callback) that converts
  the report to a format string and arguments, or directly to a string. See the
  type definition of `t:report_cb/0`, and section
  [Log Message](logger_chapter.md#log-message) in the User's Guide for more
  information about report callbacks.
""".
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
-doc "A unique identifier for a filter.".
-type filter_id() :: atom().
-doc """
A filter which can be installed as a handler filter, or as a primary filter in
Logger.
""".
-type filter() :: {fun((log_event(),filter_arg()) ->
                              filter_return()),filter_arg()}.
-doc "The second argument to the filter fun.".
-type filter_arg() :: term().
-doc "The return value from the filter fun.".
-type filter_return() :: stop | ignore | log_event().
-doc """
Primary configuration data for Logger. The following default values apply:

- `level => info`
- `filter_default => log`
- `filters => []`
""".
-type primary_config() :: #{level => level() | all | none,
                            metadata => metadata(),
                            filter_default => log | stop,
                            filters => [{filter_id(),filter()}]}.
-doc "A timestamp produced with [`logger:timestamp()`](`timestamp/0`).".
-type timestamp() :: integer().
-doc """
Configuration data for the formatter. See `m:logger_formatter` for an example of
a formatter implementation.
""".
-type formatter_config() :: #{atom() => term()}.

-doc """
Configuration used when adding or updating a handler.
""".
-type config_handler() :: {handler, logger_handler:id(), module(), logger_handler:config()}.

-type config_logger() :: [{handler,default,undefined} |
                          config_handler() |
                          {filters,log | stop,[{filter_id(),filter()}]} |
                          {module_level,level(),[module()]}].

-doc "Overload protection configuration.

> #### Note {: .info }
>
> DEPRECATED: Use `t:logger_handler:olp_config/0` instead.
".
-type olp_config() :: logger_handler:olp_config().

%% Kept for backwards compatibility
-doc """
A unique identifier for a handler instance.

> #### Note {: .info }
>
> DEPRECATED: Use `t:logger_handler:id/0` instead.
""".
-type handler_id() :: logger_handler:id().
-doc """
Handler configuration data for Logger.

> #### Note {: .info }
>
> DEPRECATED: Use `t:logger_handler:config/0` instead.
""".
-type handler_config() :: logger_handler:config().

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

-define(LOG_DOC_1(Level), (#{equiv => Level(StringOrReport, #{})})).
-define(LOG_DOC_2(Level), "
Create a " Level " log event.

Equivalent to [`log(" Level ", StringOrReport, Metadata)`](`log/3`) if called
as [`" Level "(StringOrReport, Metadata)`](`" Level "/2`).

Equivalent to [`" Level "(FormatOrFun, Args, #{})`](`" Level "/3`) if called as
[`" Level "(FormatOrFun, Args)`](`" Level "/2`).
").
-define(LOG_DOC_3(Level), #{equiv => log(Level, FormatOrFun, Args, Metadata) } ).

-doc ?LOG_DOC_1(emergency).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec emergency(String :: unicode:chardata()) -> ok;
               (Report :: report()) -> ok.
emergency(StringOrReport) ->
    log(emergency,StringOrReport).

-doc ?LOG_DOC_2("emergency").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec emergency(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
               (Report :: report(), Metadata :: metadata()) -> ok;
               (Format :: io:format(), Args :: [term()]) -> ok;
               (Fun :: msg_fun(), FunArgs :: term()) -> ok.
emergency(FormatOrFun,Args) ->
    log(emergency,FormatOrFun,Args).

-doc ?LOG_DOC_3(emergency).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec emergency(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
               (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
emergency(FormatOrFun,Args,Metadata) ->
    log(emergency,FormatOrFun,Args,Metadata).

-doc ?LOG_DOC_1(alert).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec alert(String :: unicode:chardata()) -> ok;
           (Report :: report()) -> ok.
alert(StringOrReport) ->
    log(alert,StringOrReport).
-doc ?LOG_DOC_2("alert").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec alert(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
           (Report :: report(), Metadata :: metadata()) -> ok;
           (Format :: io:format(), Args :: [term()]) -> ok;
           (Fun :: msg_fun(), FunArgs :: term()) -> ok.
alert(FormatOrFun,Args) ->
    log(alert,FormatOrFun,Args).
-doc ?LOG_DOC_3(alert).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec alert(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
           (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
alert(FormatOrFun,Args,Metadata) ->
    log(alert,FormatOrFun,Args,Metadata).

-doc ?LOG_DOC_1(critical).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec critical(String :: unicode:chardata()) -> ok;
              (Report :: report()) -> ok.
critical(StringOrReport) ->
    log(critical,StringOrReport).
-doc ?LOG_DOC_2("critical").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec critical(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
              (Report :: report(), Metadata :: metadata()) -> ok;
              (Format :: io:format(), Args :: [term()]) -> ok;
              (Fun :: msg_fun(), FunArgs :: term()) -> ok.
critical(FormatOrFun,Args) ->
    log(critical,FormatOrFun,Args).
-doc ?LOG_DOC_3(critical).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec critical(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
              (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
critical(FormatOrFun,Args,Metadata) ->
    log(critical,FormatOrFun,Args,Metadata).

-doc ?LOG_DOC_1(error).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec error(String :: unicode:chardata()) -> ok;
           (Report :: report()) -> ok.
error(StringOrReport) ->
    log(error,StringOrReport).
-doc ?LOG_DOC_2("error").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec error(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
           (Report :: report(), Metadata :: metadata()) -> ok;
           (Format :: io:format(), Args :: [term()]) -> ok;
           (Fun :: msg_fun(), FunArgs :: term()) -> ok.
error(FormatOrFun,Args) ->
    log(error,FormatOrFun,Args).
-doc ?LOG_DOC_3(error).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec error(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
           (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
error(FormatOrFun,Args,Metadata) ->
    log(error,FormatOrFun,Args,Metadata).

-doc ?LOG_DOC_1(warning).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec warning(String :: unicode:chardata()) -> ok;
             (Report :: report()) -> ok.
warning(StringOrReport) ->
    log(warning,StringOrReport).
-doc ?LOG_DOC_2("warning").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec warning(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
             (Report :: report(), Metadata :: metadata()) -> ok;
             (Format :: io:format(), Args :: [term()]) -> ok;
             (Fun :: msg_fun(), FunArgs :: term()) -> ok.
warning(FormatOrFun,Args) ->
    log(warning,FormatOrFun,Args).
-doc ?LOG_DOC_3(warning).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec warning(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
             (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
warning(FormatOrFun,Args,Metadata) ->
    log(warning,FormatOrFun,Args,Metadata).

-doc ?LOG_DOC_1(notice).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec notice(String :: unicode:chardata()) -> ok;
            (Report :: report()) -> ok.
notice(StringOrReport) ->
    log(notice,StringOrReport).
-doc ?LOG_DOC_2("notice").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec notice(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
            (Report :: report(), Metadata :: metadata()) -> ok;
            (Format :: io:format(), Args :: [term()]) -> ok;
            (Fun :: msg_fun(), FunArgs :: term()) -> ok.
notice(FormatOrFun,Args) ->
    log(notice,FormatOrFun,Args).
-doc ?LOG_DOC_3(notice).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec notice(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
            (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
notice(FormatOrFun,Args,Metadata) ->
    log(notice,FormatOrFun,Args,Metadata).

-doc ?LOG_DOC_1(info).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec info(String :: unicode:chardata()) -> ok;
          (Report :: report()) -> ok.
info(StringOrReport) ->
    log(info,StringOrReport).
-doc ?LOG_DOC_2("info").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec info(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
          (Report :: report(), Metadata :: metadata()) -> ok;
          (Format :: io:format(), Args :: [term()]) -> ok;
          (Fun :: msg_fun(), FunArgs :: term()) -> ok.
info(FormatOrFun,Args) ->
    log(info,FormatOrFun,Args).
-doc ?LOG_DOC_3(info).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec info(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
          (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
info(FormatOrFun,Args,Metadata) ->
    log(info,FormatOrFun,Args,Metadata).

-doc ?LOG_DOC_1(debug).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec debug(String :: unicode:chardata()) -> ok;
           (Report :: report()) -> ok.
debug(StringOrReport) ->
    log(debug,StringOrReport).
-doc ?LOG_DOC_2("debug").
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec debug(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
           (Report :: report(), Metadata :: metadata()) -> ok;
           (Format :: io:format(), Args :: [term()]) -> ok;
           (Fun :: msg_fun(), FunArgs :: term()) -> ok.
debug(FormatOrFun,Args) ->
    log(debug,FormatOrFun,Args).
-doc ?LOG_DOC_3(debug).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec debug(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
           (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
debug(FormatOrFun,Args,Metadata) ->
    log(debug,FormatOrFun,Args,Metadata).

-doc(#{equiv => log(Level, StringOrReport, #{})}).
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec log(Level :: level(), String :: unicode:chardata()) -> ok;
         (Level :: level(), Report :: report()) -> ok.
log(Level, StringOrReport) ->
    do_log(Level,StringOrReport,#{}).

-doc """
Create a log event at the given [log level](logger_chapter.md#log-level), with
the given [message](logger_chapter.md#log-message) to be logged and
[_metadata_](logger_chapter.md#metadata).

*Example*:

```erlang
%% A plain string
1> logger:log(info, "Hello World").
%% A plain string with metadata
2> logger:log(debug, "Hello World", #{ meta => data }).
%% A format string with arguments
3> logger:log(warning, "The roof is on ~ts",[Cause]).
%% A report
4> logger:log(warning, #{ what => roof, cause => Cause }).
```

Equivalent to [`log(Level, FormatOrFun, Args, #{})`](`log/4`) if called as
`log(Level, FormatOrFun, Args)`.
""".
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec log(Level :: level(), String :: unicode:chardata(), Metadata :: metadata()) -> ok;
         (Level :: level(), Report :: report(), Metadata :: metadata()) -> ok;
         (Level :: level(), Format :: io:format(), Args :: [term()]) -> ok;
         (Level :: level(), Fun :: msg_fun(), FunArgs :: term()) -> ok.
log(Level, StringOrReport, Metadata)
  when is_map(Metadata), not is_function(StringOrReport) ->
    do_log(Level,StringOrReport,Metadata);
log(Level, FunOrFormat, Args) ->
    do_log(Level,{FunOrFormat,Args},#{}).

-doc """
Create a log event at the given [log level](logger_chapter.md#log-level), with
the given [message](logger_chapter.md#log-message) to be logged and
[_metadata_](logger_chapter.md#metadata).

The message and metadata can either be given directly in the arguments, or
returned from a fun. Passing a fun instead of the message/metadata directly is
useful in scenarios when the message/metadata is very expensive to compute. This
is because the fun is only evaluated when the message/metadata is actually
needed, which may be not at all if the log event is not to be logged. Examples:

```erlang
%% A plain string with expensive metadata
1> logger:info(fun([]) -> {"Hello World", #{ meta => expensive() }} end,[]).
%% An expensive report
2> logger:debug(fun(What) -> #{ what => What, cause => expensive() } end,roof).
%% A plain string with expensive metadata and normal metadata
3> logger:debug(fun([]) -> {"Hello World", #{ meta => expensive() }} end,[],
               #{ meta => data }).
```

When metadata is given both as an argument and returned from the fun they are
merged. If equal keys exists the values are taken from the metadata returned by
the fun.
""".
-doc(#{group => <<"Logging API functions">>,since => <<"OTP 21.0">>}).
-spec log(Level :: level(), Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
         (Level :: level(), Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
log(Level, FunOrFormat, Args, Metadata) ->
    do_log(Level,{FunOrFormat,Args},Metadata).

-doc false.
-spec allow(Level,Module) -> boolean() when
      Level :: level(),
      Module :: module().
allow(Level,Module) when is_atom(Module) ->
    logger_config:allow(Level,Module).

-doc false.
-spec macro_log(Location,Level,StringOrReport)  -> ok when
      Location :: location(),
      Level :: level(),
      StringOrReport :: unicode:chardata() | report().
macro_log(Location,Level,StringOrReport) ->
    log_allowed(Location,Level,StringOrReport,#{}).

-doc false.
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

-doc false.
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

-doc false.
-spec format_otp_report(Report) -> FormatArgs when
      Report :: report(),
      FormatArgs :: {io:format(),[term()]}.
format_otp_report(#{label:=_,report:=Report}) ->
    format_report(Report);
format_otp_report(Report) ->
    format_report(Report).

-doc """
Convert a log message on report form to `{Format, Args}`. This is the default
report callback used by `m:logger_formatter` when no custom report callback is
found. See section [Log Message](logger_chapter.md#log-message) in the Kernel
User's Guide for information about report callbacks and valid forms of log
messages.

The function produces lines of `Key: Value` from key-value lists. Strings are
printed with `~ts` and other terms with `~tp`.

If `Report` is a map, it is converted to a key-value list before formatting as
such.
""".
-doc(#{group => <<"Miscellaneous API functions">>,since => <<"OTP 21.0">>}).
-spec format_report(Report) -> FormatArgs when
      Report :: report(),
      FormatArgs :: {io:format(),[term()]}.
format_report(Report) when is_map(Report) ->
    format_report(maps:to_list(Report));
format_report(Report) ->
    try lists:flatten(Report) of
        [] ->
            {"~tp",[[]]};
        FlatList ->
            case string_p1(FlatList) of
                true ->
                    {"~ts",[FlatList]};
                false ->
                    format_term_list(Report,[],[])
            end
    catch _:_ ->
            {"~tp",[Report]}
    end.

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

string_p(List) ->
    try lists:flatten(List) of
        FlatList ->
            string_p1(FlatList)
    catch _:_ ->
            false
    end.

string_p1([]) ->
    false;
string_p1(FlatList) ->
    io_lib:printable_unicode_list(FlatList).

-doc false.
internal_log(Level,Term) when is_atom(Level) ->
    erlang:display_string("Logger - "++ atom_to_list(Level) ++ ": "),
    erlang:display(Term).

-doc """
Return a timestamp that can be inserted as the `time` field in the meta data for
a log event. It is produced with
[`os:system_time(microsecond)`](`os:system_time/1`).

Notice that Logger automatically inserts a timestamp in the meta data unless it
already exists. This function is exported for the rare case when the timestamp
must be taken at a different point in time than when the log event is issued.
""".
-doc(#{group => <<"Miscellaneous API functions">>,since => <<"OTP 21.3">>}).
-spec timestamp() -> timestamp().
timestamp() ->
    os:system_time(microsecond).

%%%-----------------------------------------------------------------
%%% Configuration
-doc """
Add a primary filter to Logger.

The filter fun is called with the log event as the first parameter, and the
specified `filter_args()` as the second parameter.

The return value of the fun specifies if a log event is to be discarded or
forwarded to the handlers:

- **`t:log_event/0`** - The filter _passed_. The next primary filter, if any, is
  applied. If no more primary filters exist, the log event is forwarded to the
  handler part of Logger, where handler filters are applied.

- **`stop`** - The filter _did not pass_, and the log event is immediately
  discarded.

- **`ignore`** - The filter has no knowledge of the log event. The next primary
  filter, if any, is applied. If no more primary filters exist, the value of the
  primary `filter_default` configuration parameter specifies if the log event
  shall be discarded or forwarded to the handler part.

See section [Filters](logger_chapter.md#filters) in the User's Guide for more
information about filters.

Some built-in filters exist. These are defined in `m:logger_filters`.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec add_primary_filter(FilterId,Filter) -> ok | {error,term()} when
      FilterId :: filter_id(),
      Filter :: filter().
add_primary_filter(FilterId,Filter) ->
    logger_server:add_filter(primary,{FilterId,Filter}).

-doc """
Add a filter to the specified handler.

The filter fun is called with the log event as the first parameter, and the
specified `filter_args()` as the second parameter.

The return value of the fun specifies if a log event is to be discarded or
forwarded to the handler callback:

- **`t:log_event/0`** - The filter _passed_. The next handler filter, if any, is
  applied. If no more filters exist for this handler, the log event is forwarded
  to the handler callback.

- **`stop`** - The filter _did not pass_, and the log event is immediately
  discarded.

- **`ignore`** - The filter has no knowledge of the log event. The next handler
  filter, if any, is applied. If no more filters exist for this handler, the
  value of the `filter_default` configuration parameter for the handler
  specifies if the log event shall be discarded or forwarded to the handler
  callback.

See section [Filters](logger_chapter.md#filters) in the User's Guide for more
information about filters.

Some built-in filters exist. These are defined in `m:logger_filters`.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec add_handler_filter(HandlerId,FilterId,Filter) -> ok | {error,term()} when
      HandlerId :: logger_handler:id(),
      FilterId :: filter_id(),
      Filter :: filter().
add_handler_filter(HandlerId,FilterId,Filter) ->
    logger_server:add_filter(HandlerId,{FilterId,Filter}).


-doc "Remove the primary filter identified by `FilterId` from Logger.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec remove_primary_filter(FilterId) -> ok | {error,term()} when
      FilterId :: filter_id().
remove_primary_filter(FilterId) ->
    logger_server:remove_filter(primary,FilterId).

-doc """
Remove the filter identified by `FilterId` from the handler identified by
`HandlerId`.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec remove_handler_filter(HandlerId,FilterId) -> ok | {error,term()} when
      HandlerId :: logger_handler:id(),
      FilterId :: filter_id().
remove_handler_filter(HandlerId,FilterId) ->
    logger_server:remove_filter(HandlerId,FilterId).

-doc """
Add a handler with the given configuration.

`HandlerId` is a unique identifier which must be used in all subsequent calls
referring to this handler.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec add_handler(HandlerId,Module,Config) -> ok | {error,term()} when
      HandlerId :: logger_handler:id(),
      Module :: module(),
      Config :: logger_handler:config().
add_handler(HandlerId,Module,Config) ->
    logger_server:add_handler(HandlerId,Module,Config).

-doc "Remove the handler identified by `HandlerId`.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec remove_handler(HandlerId) -> ok | {error,term()} when
      HandlerId :: logger_handler:id().
remove_handler(HandlerId) ->
    logger_server:remove_handler(HandlerId).

-doc """
Add or update primary configuration data for Logger. If the given `Key` already
exists, its associated value will be changed to the given value. If it does not
exist, it will be added.

The `metadata` key was added in OTP 24.0.
""".
-doc(#{group => <<"Configuration API functions">>,
       since => <<"OTP 21.0">>}).
-spec set_primary_config(level,Level) -> ok | {error,term()} when
      Level :: level() | all | none;
                        (filter_default,FilterDefault) -> ok | {error,term()} when
      FilterDefault :: log | stop;
                        (filters,Filters) -> ok | {error,term()} when
      Filters :: [{filter_id(),filter()}];
                        (metadata,Meta) -> ok | {error,term()} when
      Meta :: metadata().
set_primary_config(Key,Value) ->
    logger_server:set_config(primary,Key,Value).

-doc """
Set primary configuration data for Logger. This overwrites the current
configuration.

To modify the existing configuration, use `update_primary_config/1`, or, if a
more complex merge is needed, read the current configuration with
[`get_primary_config/0` ](`get_primary_config/0`), then do the merge before
writing the new configuration back with this function.

If a key is removed compared to the current configuration, the default value is
used.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec set_primary_config(Config) -> ok | {error,term()} when
      Config :: primary_config().
set_primary_config(Config) ->
    logger_server:set_config(primary,Config).


-doc """
Add or update configuration data for the specified handler. If the given `Key`
already exists, its associated value will be changed to the given value. If it
does not exist, it will be added.

If the value is incomplete, which for example can be the case for the `config`
key, it is up to the handler implementation how the unspecified parts are set.
For all handlers in the Kernel application, unspecified data for the `config`
key is set to default values. To update only specified data, and keep the
existing configuration for the rest, use `update_handler_config/3`.

See the definition of the `t:logger_handler:config/0` type for more information
about the different parameters.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec set_handler_config(HandlerId,level,Level) -> Return when
      HandlerId :: logger_handler:id(),
      Level :: level() | all | none,
      Return :: ok | {error,term()};
                        (HandlerId,filter_default,FilterDefault) -> Return when
      HandlerId :: logger_handler:id(),
      FilterDefault :: log | stop,
      Return :: ok | {error,term()};
                        (HandlerId,filters,Filters) -> Return when
      HandlerId :: logger_handler:id(),
      Filters :: [{filter_id(),filter()}],
      Return :: ok | {error,term()};
                        (HandlerId,formatter,Formatter) -> Return when
      HandlerId :: logger_handler:id(),
      Formatter :: {module(), formatter_config()},
      Return :: ok | {error,term()};
                        (HandlerId,config,Config) -> Return when
      HandlerId :: logger_handler:id(),
      Config :: term(),
      Return :: ok | {error,term()}.
set_handler_config(HandlerId,Key,Value) ->
    logger_server:set_config(HandlerId,Key,Value).

-doc """
Set configuration data for the specified handler. This overwrites the current
handler configuration.

To modify the existing configuration, use `update_handler_config/2`, or, if a
more complex merge is needed, read the current configuration with
[`get_handler_config/1` ](`get_handler_config/1`), then do the merge before
writing the new configuration back with this function.

If a key is removed compared to the current configuration, and the key is known
by Logger, the default value is used. If it is a custom key, then it is up to
the handler implementation if the value is removed or a default value is
inserted.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec set_handler_config(HandlerId,Config) -> ok | {error,term()} when
      HandlerId :: logger_handler:id(),
      Config :: logger_handler:config().
set_handler_config(HandlerId,Config) ->
    logger_server:set_config(HandlerId,Config).

-doc """
Set configuration data for the Logger proxy. This overwrites the current proxy
configuration. Keys that are not specified in the `Config` map gets default
values.

To modify the existing configuration, use `update_proxy_config/1`, or, if a more
complex merge is needed, read the current configuration with
[`get_proxy_config/0` ](`get_proxy_config/0`), then do the merge before writing
the new configuration back with this function.

For more information about the proxy, see section
[Logger Proxy](logger_chapter.md#logger-proxy) in the Kernel User's Guide.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.3">>}).
-spec set_proxy_config(Config) -> ok | {error,term()} when
      Config :: olp_config().
set_proxy_config(Config) ->
    logger_server:set_config(proxy,Config).

-doc """
Update primary configuration data for Logger. This function behaves as if it was
implemented as follows:

```erlang
Old = logger:get_primary_config(),
logger:set_primary_config(maps:merge(Old, Config)).
```

To overwrite the existing configuration without any merge, use
[`set_primary_config/1` ](`set_primary_config/1`).
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec update_primary_config(Config) -> ok | {error,term()} when
      Config :: primary_config().
update_primary_config(Config) ->
    logger_server:update_config(primary,Config).

-doc """
Add or update configuration data for the specified handler. If the given `Key`
already exists, its associated value will be changed to the given value. If it
does not exist, it will be added.

If the value is incomplete, which for example can be the case for the `config`
key, it is up to the handler implementation how the unspecified parts are set.
For all handlers in the Kernel application, unspecified data for the `config`
key is not changed. To reset unspecified data to default values, use
`set_handler_config/3`.

See the definition of the `t:logger_handler:config/0` type for more information
about the different parameters.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.2">>}).
-spec update_handler_config(HandlerId,level,Level) -> Return when
      HandlerId :: logger_handler:id(),
      Level :: level() | all | none,
      Return :: ok | {error,term()};
                        (HandlerId,filter_default,FilterDefault) -> Return when
      HandlerId :: logger_handler:id(),
      FilterDefault :: log | stop,
      Return :: ok | {error,term()};
                        (HandlerId,filters,Filters) -> Return when
      HandlerId :: logger_handler:id(),
      Filters :: [{filter_id(),filter()}],
      Return :: ok | {error,term()};
                        (HandlerId,formatter,Formatter) -> Return when
      HandlerId :: logger_handler:id(),
      Formatter :: {module(), formatter_config()},
      Return :: ok | {error,term()};
                        (HandlerId,config,Config) -> Return when
      HandlerId :: logger_handler:id(),
      Config :: term(),
      Return :: ok | {error,term()}.
update_handler_config(HandlerId,Key,Value) ->
    logger_server:update_config(HandlerId,Key,Value).

-doc """
Update configuration data for the specified handler. This function behaves as if
it was implemented as follows:

```erlang
{ok, {_, Old}} = logger:get_handler_config(HandlerId),
logger:set_handler_config(HandlerId, maps:merge(Old, Config)).
```

To overwrite the existing configuration without any merge, use
[`set_handler_config/2` ](`set_handler_config/2`).
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec update_handler_config(HandlerId,Config) -> ok | {error,term()} when
      HandlerId :: logger_handler:id(),
      Config :: logger_handler:config().
update_handler_config(HandlerId,Config) ->
    logger_server:update_config(HandlerId,Config).

-doc """
Update configuration data for the Logger proxy. This function behaves as if it
was implemented as follows:

```erlang
Old = logger:get_proxy_config(),
logger:set_proxy_config(maps:merge(Old, Config)).
```

To overwrite the existing configuration without any merge, use
[`set_proxy_config/1` ](`set_proxy_config/1`).

For more information about the proxy, see section
[Logger Proxy](logger_chapter.md#logger-proxy) in the Kernel User's Guide.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.3">>}).
-spec update_proxy_config(Config) -> ok | {error,term()} when
      Config :: olp_config().
update_proxy_config(Config) ->
    logger_server:update_config(proxy,Config).

-doc "Look up the current primary configuration for Logger.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_primary_config() -> Config when
      Config :: primary_config().
get_primary_config() ->
    {ok,Config} = logger_config:get(?LOGGER_TABLE,primary),
    maps:remove(handlers,Config).

-doc "Look up the current configuration for the given handler.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_handler_config(HandlerId) -> {ok,Config} | {error,term()} when
      HandlerId :: logger_handler:id(),
      Config :: logger_handler:config().
get_handler_config(HandlerId) ->
    case logger_config:get(?LOGGER_TABLE,HandlerId) of
        {ok,#{module:=Module}=Config} ->
            {ok,try Module:filter_config(Config)
                catch _:_ -> Config
                end};
        Error ->
            Error
    end.

-doc "Look up the current configuration for all handlers.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_handler_config() -> [Config] when
      Config :: logger_handler:config().
get_handler_config() ->
    [begin
         {ok,Config} = get_handler_config(HandlerId),
         Config
     end || HandlerId <- get_handler_ids()].

-doc "Look up the identities for all installed handlers.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_handler_ids() -> [HandlerId] when
      HandlerId :: logger_handler:id().
get_handler_ids() ->
    {ok,#{handlers:=HandlerIds}} = logger_config:get(?LOGGER_TABLE,primary),
    HandlerIds.

-doc """
Look up the current configuration for the Logger proxy.

For more information about the proxy, see section
[Logger Proxy](logger_chapter.md#logger-proxy) in the Kernel User's Guide.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.3">>}).
-spec get_proxy_config() -> Config when
      Config :: olp_config().
get_proxy_config() ->
    {ok,Config} = logger_config:get(?LOGGER_TABLE,proxy),
    Config.

-doc """
Update the formatter configuration for the specified handler.

The new configuration is merged with the existing formatter configuration.

To overwrite the existing configuration without any merge, use

```erlang
set_handler_config(HandlerId, formatter,
	      {FormatterModule, FormatterConfig}).
```
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec update_formatter_config(HandlerId,FormatterConfig) ->
                                     ok | {error,term()} when
      HandlerId :: logger_handler:id(),
      FormatterConfig :: formatter_config().
update_formatter_config(HandlerId,FormatterConfig) ->
    logger_server:update_formatter_config(HandlerId,FormatterConfig).

-doc(#{ equiv => update_formatter_config(HandlerId, #{ Key => Value })}).
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec update_formatter_config(HandlerId,Key,Value) ->
                                     ok | {error,term()} when
      HandlerId :: logger_handler:id(),
      Key :: atom(),
      Value :: term().
update_formatter_config(HandlerId,Key,Value) ->
    logger_server:update_formatter_config(HandlerId,#{Key=>Value}).

-doc """
Set the log level for the specified modules.

The log level for a module overrides the primary log level of Logger for log
events originating from the module in question. Notice, however, that it does
not override the level configuration for any handler.

For example: Assume that the primary log level for Logger is `info`, and there
is one handler, `h1`, with level `info` and one handler, `h2`, with level
`debug`.

With this configuration, no debug messages will be logged, since they are all
stopped by the primary log level.

If the level for `mymodule` is now set to `debug`, then debug events from this
module will be logged by the handler `h2`, but not by handler `h1`.

Debug events from other modules are still not logged.

To change the primary log level for Logger, use
[`set_primary_config(level, Level)`](`set_primary_config/2`).

To change the log level for a handler, use
[`set_handler_config(HandlerId, level, Level)` ](`set_handler_config/3`).

> #### Note {: .info }
>
> The originating module for a log event is only detected if the key `mfa`
> exists in the metadata, and is associated with `{Module, Function, Arity}`.
> When log macros are used, this association is automatically added to all log
> events. If an API function is called directly, without using a macro, the
> logging client must explicitly add this information if module levels shall
> have any effect.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec set_module_level(Modules,Level) -> ok | {error,term()} when
      Modules :: [module()] | module(),
      Level :: level() | all | none.
set_module_level(Module,Level) when is_atom(Module) ->
    set_module_level([Module],Level);
set_module_level(Modules,Level) ->
    logger_server:set_module_level(Modules,Level).

-doc """
Remove module specific log settings. After this, the primary log level is used
for the specified modules.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec unset_module_level(Modules) -> ok when
      Modules :: [module()] | module().
unset_module_level(Module) when is_atom(Module) ->
    unset_module_level([Module]);
unset_module_level(Modules) ->
    logger_server:unset_module_level(Modules).

-doc """
Remove module specific log settings. After this, the primary log level is used
for all modules.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec unset_module_level() -> ok.
unset_module_level() ->
    logger_server:unset_module_level().

-doc """
Set the log level for all the modules of the specified application.

This function is a convenience function that calls
[logger:set_module_level/2](`set_module_level/2`) for each module associated
with an application.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.1">>}).
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

-doc """
Unset the log level for all the modules of the specified application.

This function is a utility function that calls
[logger:unset_module_level/2](`unset_module_level/1`) for each module associated
with an application.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.1">>}).
-spec unset_application_level(Application) ->
         ok | {error, {not_loaded, Application}} when Application :: atom().
unset_application_level(App) ->
    case application:get_key(App, modules) of
        {ok, Modules} ->
            unset_module_level(Modules);
        undefined ->
            {error, {not_loaded, App}}
    end.

-doc """
Look up the current level for the given modules. Returns a list containing one
`{Module,Level}` element for each of the given modules for which the module
level was previously set with `set_module_level/2`.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_module_level(Modules) -> [{Module,Level}] when
      Modules :: [Module] | Module,
      Module :: module(),
      Level :: level() | all | none.
get_module_level(Module) when is_atom(Module) ->
    get_module_level([Module]);
get_module_level(Modules) when is_list(Modules) ->
    [{M,L} || {M,L} <- get_module_level(),
              lists:member(M,Modules)].

-doc """
Look up all current module levels. Returns a list containing one
`{Module,Level}` element for each module for which the module level was
previously set with `set_module_level/2`.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_module_level() -> [{Module,Level}] when
      Module :: module(),
      Level :: level() | all | none.
get_module_level() ->
    logger_config:get_module_level().

%%%-----------------------------------------------------------------
%%% Misc
-doc """
Compare the severity of two log levels. Returns `gt` if `Level1` is more severe
than `Level2`, `lt` if `Level1` is less severe, and `eq` if the levels are
equal.
""".
-doc(#{group => <<"Miscellaneous API functions">>,since => <<"OTP 21.0">>}).
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

-doc """
Set metadata which Logger shall automatically insert in all log events produced
on the current process.

Location data produced by the log macros, and/or metadata given as argument to
the log call (API function or macro), are merged with the process metadata. If
the same keys occur, values from the metadata argument to the log call overwrite
values from the process metadata, which in turn overwrite values from the
location data.

Subsequent calls to this function overwrites previous data set. To update
existing data instead of overwriting it, see `update_process_metadata/1`.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec set_process_metadata(Meta) -> ok when
      Meta :: metadata().
set_process_metadata(Meta) when is_map(Meta) ->
    _ = put(?LOGGER_META_KEY,Meta),
    ok;
set_process_metadata(Meta) ->
    erlang:error(badarg,[Meta]).

-doc """
Set or update metadata to use when logging from current process

If process metadata exists for the current process, this function behaves as if
it was implemented as follows:

```erlang
logger:set_process_metadata(maps:merge(logger:get_process_metadata(), Meta)).
```

If no process metadata exists, the function behaves as
[`set_process_metadata/1` ](`set_process_metadata/1`).
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
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

-doc "Retrieve data set with `set_process_metadata/1` or `update_process_metadata/1`.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_process_metadata() -> Meta | undefined when
      Meta :: metadata().
get_process_metadata() ->
    get(?LOGGER_META_KEY).

-doc "Delete data set with `set_process_metadata/1` or `update_process_metadata/1`.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec unset_process_metadata() -> ok.
unset_process_metadata() ->
    _ = erase(?LOGGER_META_KEY),
    ok.

-doc """
Look up all current Logger configuration, including primary, handler, and proxy
configuration, and module level settings.
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
-spec get_config() -> #{primary=>primary_config(),
                        handlers=>[logger_handler:config()],
                        proxy=>olp_config(),
                        module_levels=>[{module(),level() | all | none}]}.
get_config() ->
    #{primary=>get_primary_config(),
      handlers=>get_handler_config(),
      proxy=>get_proxy_config(),
      module_levels=>lists:keysort(1,get_module_level())}.

-doc "Pretty print all Logger configuration.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.3">>}).
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

-doc "Pretty print the Logger configuration.".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.3">>}).
-spec i(What) -> ok when
      What :: primary | handlers | proxy | modules | logger_handler:id().
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

-doc """
Reconfigure Logger using updated `kernel` configuration that was set after
`kernel` application was loaded.

Beware, that this is meant to be run only by the build tools, not manually
during application lifetime, as this may cause missing log entries.
""".
-doc(#{group => <<"Miscellaneous API functions">>,since => <<"OTP 24.2">>}).
-spec reconfigure() -> ok | {error,term()}.
%% This function is meant to be used by the build tools like Rebar3 or Mix
%% to ensure that the logger configuration is reset to the expected state
%% before running main application.
reconfigure() ->
    try
        [case logger:remove_handler(Id) of
             ok -> ok;
             {error, Reason} -> throw({remove, Id, Reason})
         end || #{id := Id} <- logger:get_handler_config()],
        ok=logger:add_handler(simple,logger_simple_h,
                              #{filter_default=>stop,
                                filters=>?DEFAULT_HANDLER_FILTERS}),
        logger:unset_module_level(),
        internal_init_logger()
    of
        ok ->
            logger:add_handlers(kernel);
        Error ->
            Error
    catch
        throw:Reason ->
            {error, Reason}
    end.

-doc false.
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
        ok = logger:set_primary_config(metadata, get_primary_metadata()),
        ok = logger:set_primary_config(filter_default,
                                       get_primary_filter_default(Env)),
        ok = logger:set_primary_config(filters, []),

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

-doc """
Reads the application configuration parameter `logger` and calls
[`add_handlers/1`](`add_handlers/1`) with its contents.

This function should be used by custom Logger handlers to make configuration
consistent no matter which handler the system uses. Normal usage is to add a
call to `logger:add_handlers/1` just after the processes that the handler needs
are started, and pass the application's `logger` configuration as the argument.
For example:

```erlang
-behaviour(application).
start(_, []) ->
    case supervisor:start_link({local, my_sup}, my_sup, []) of
        {ok, Pid} ->
            ok = logger:add_handlers(my_app),
            {ok, Pid, []};
        Error -> Error
     end.
```

This reads the `logger` configuration parameter from the `my_app` application
and starts the configured handlers. The contents of the configuration use the
same rules as the
[logger handler configuration](logger_chapter.md#handler-configuration).

If the handler is meant to replace the default handler, the Kernel's default
handler have to be disabled before the new handler is added. A `sys.config` file
that disables the Kernel handler and adds a custom handler could look like this:

```erlang
[{kernel,
  [{logger,
    %% Disable the default Kernel handler
    [{handler, default, undefined}]}]},
 {my_app,
  [{logger,
    %% Enable this handler as the default
    [{handler, default, my_handler, #{}}]}]}].
```
""".
-doc(#{group => <<"Configuration API functions">>,since => <<"OTP 21.0">>}).
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
    add_handlers(undefined,HandlerConfig).

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

get_primary_metadata() ->
    case application:get_env(kernel,logger_metadata) of
        {ok, Meta} when is_map(Meta) ->
            Meta;
        {ok, Meta} ->
            throw({logger_metadata, Meta});
        undefined ->
            %% This case is here to keep bug compatibility. Can be removed in OTP 25.
            case application:get_env(kernel,logger_default_metadata,#{}) of
                Meta when is_map(Meta) ->
                    Meta;
                Meta ->
                    throw({logger_metadata, Meta})
            end
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
    case logger_config:allow(Level,Module) of
        true ->
            log_allowed(#{},Level,Msg,Meta);
        false ->
            ok
    end;
do_log(Level,Msg,Meta) ->
    case logger_config:allow(Level) of
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
log_allowed(Location,Level,{Fun,FunArgs}=FunCall,Meta) when is_function(Fun,1) ->
    try Fun(FunArgs) of
        {FunRes, #{} = FunMeta} ->
            log_fun_allowed(Location, Level, FunRes, maps:merge(Meta, FunMeta), FunCall);
        FunRes ->
            log_fun_allowed(Location, Level, FunRes, Meta, FunCall)
    catch C:R ->
            log_allowed(Location,Level,
                        {"LAZY_FUN CRASH: ~tp; Reason: ~tp",
                         [{Fun,FunArgs},{C,R}]},
                        Meta)
    end;
log_allowed(Location,Level,Msg,LogCallMeta) when is_map(LogCallMeta) ->

    Tid = tid(),

    {ok,PrimaryConfig} = logger_config:get(Tid,primary),

    %% Metadata priorities are:
    %% Location (added in API macros) - will be overwritten by
    %% Default Metadata (added by logger:primary_config/1) - will be overwritten by
    %% process metadata (set by set_process_metadata/1), which in turn will be
    %% overwritten by the metadata given as argument in the log call
    %% (function or macro).

    Meta = add_default_metadata(
             maps:merge(
               Location,
               maps:merge(
                 maps:get(metadata,PrimaryConfig),
                 maps:merge(proc_meta(),LogCallMeta)))),

    case node(maps:get(gl,Meta)) of
        Node when Node=/=node() ->
            log_remote(Node,Level,Msg,Meta);
        _ ->
            ok
    end,
    do_log_allowed(Level,Msg,Meta,Tid,PrimaryConfig).

log_fun_allowed(Location, Level, FunRes, Meta, FunCall) ->
    case FunRes of
        ignore ->
            ok;
        Msg={Format,Args} when ?IS_FORMAT(Format), is_list(Args) ->
            log_allowed(Location,Level,Msg,Meta);
        Report when ?IS_REPORT(Report) ->
            log_allowed(Location,Level,Report,Meta);
        String when ?IS_STRING(String) ->
            log_allowed(Location,Level,String,Meta);
        Other ->
            log_allowed(Location,Level,
                        {"LAZY_FUN ERROR: ~tp; Returned: ~tp",
                         [FunCall,Other]},
                        Meta)
    end.

do_log_allowed(Level,{Format,Args},Meta,Tid,Config)
  when ?IS_LEVEL(Level),
       ?IS_FORMAT(Format),
       is_list(Args),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>{deatomize(Format),Args},meta=>Meta},
                               Tid,Config);
do_log_allowed(Level,Report,Meta,Tid,Config)
  when ?IS_LEVEL(Level),
       ?IS_REPORT(Report),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>{report,Report},meta=>Meta},
                               Tid,Config);
do_log_allowed(Level,String,Meta,Tid,Config)
  when ?IS_LEVEL(Level),
       ?IS_STRING(String),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>{string,String},meta=>Meta},
                               Tid,Config).
tid() ->
    ets:whereis(?LOGGER_TABLE).

deatomize(Atom) when is_atom(Atom) -> atom_to_list(Atom);
deatomize(Other) -> Other.

log_remote(Node,Level,{Format,Args},Meta) ->
    log_remote(Node,{log,Level,Format,Args,Meta});
log_remote(Node,Level,Msg,Meta) ->
    log_remote(Node,{log,Level,Msg,Meta}).

log_remote(Node,Request) ->
    logger_proxy:log({remote,Node,Request}),
    ok.

-doc false.
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

%% Remove everything up to and including this module from the stacktrace
-doc false.
filter_stacktrace(Module,[{Module,_,_,_}|_]) ->
    [];
filter_stacktrace(Module,[H|T]) ->
    [H|filter_stacktrace(Module,T)];
filter_stacktrace(_,[]) ->
    [].
