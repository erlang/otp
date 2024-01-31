<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Logging Cookbook

Using and especially configuring Logger can be difficult at times as there are
many different options that can be changed and often more than one way to
achieve the same result. This User's Guide tries to help by giving many
different examples of how you can use logger.

For more examples of practical use-cases of using Logger, Fred Hebert's blog
post
[Erlang/OTP 21's new logger](https://ferd.ca/erlang-otp-21-s-new-logger.html) is
a great starting point.

> #### Note {: .info }
>
> If you find that some common Logger usage is missing from this guide, please
> open a pull request on github with the suggested addition

## Get Logger information

### Print the primary Logger configurations.

```erlang
1> logger:i(primary).
Primary configuration:
    Level: notice
    Filter Default: log
    Filters:
        (none)
```

It is also possible to fetch the configuration using
[`logger:get_primary_config()`](`logger:get_primary_config/0`).

#### See also

- [logger:i()](`logger:i/0`)
- [Configuration](logger_chapter.md#configuration) in the Logging User's Guide

### Print the configuration of all handlers.

```erlang
2> logger:i(handlers).
Handler configuration:
    Id: default
        Module: logger_std_h
        Level:  all
        Formatter:
            Module: logger_formatter
            Config:
                legacy_header: true
                single_line: false
        Filter Default: stop
        Filters:
            Id: remote_gl
                Fun: fun logger_filters:remote_gl/2
                Arg: stop
            Id: domain
                Fun: fun logger_filters:domain/2
                Arg: {log,super,[otp,sasl]}
            Id: no_domain
                Fun: fun logger_filters:domain/2
                Arg: {log,undefined,[]}
        Handler Config:
            burst_limit_enable: true
            burst_limit_max_count: 500
            burst_limit_window_time: 1000
            drop_mode_qlen: 200
            filesync_repeat_interval: no_repeat
            flush_qlen: 1000
            overload_kill_enable: false
            overload_kill_mem_size: 3000000
            overload_kill_qlen: 20000
            overload_kill_restart_after: 5000
            sync_mode_qlen: 10
            type: standard_io
```

You can also print the configuration of a specific handler using
[`logger:i(HandlerName)`](`logger:i/1`), or fetch the configuration using
[`logger:get_handler_config()`](`logger:get_handler_config/0`), or
[`logger:get_handler_config(HandlerName)`](`logger:get_handler_config/1`) for a
specific handler.

#### See also

- [`logger:i()`](`logger:i/0`)
- [Configuration](logger_chapter.md#configuration) in the Logging User's Guide

## Configure the Logger

### Where did my progress reports go?

In OTP-21 the default primary log level is `notice`. The means that many log
messages are by default not printed. This includes the progress reports of
supervisors. In order to get progress reports you need to raise the primary log
level to `info`

```erlang
$ erl -kernel logger_level info
=PROGRESS REPORT==== 4-Nov-2019::16:33:11.742069 ===
    application: kernel
    started_at: nonode@nohost
=PROGRESS REPORT==== 4-Nov-2019::16:33:11.746546 ===
    application: stdlib
    started_at: nonode@nohost
Eshell V10.5.3  (abort with ^G)
1>
```

## Configure Logger formatter

In order to fit better into your existing logging infrastructure Logger can
format its logging messages any way you want to. Either you can use the built-in
formatter, or you can build your own.

### Single line configuration

Since single line logging is the default of the built-in formatter you only have
to provide the empty map as the configuration. The example below uses the
`sys.config` to change the formatter configuration.

```erlang
$ cat sys.config
[{kernel,
  [{logger,
    [{handler, default, logger_std_h,
      #{ formatter => {logger_formatter, #{ }}}}]}]}].
$ erl -config sys
Eshell V10.5.1  (abort with ^G)
1> logger:error("Oh noes, an error").
1962-10-03T11:07:47.466763-04:00 error: Oh noes, an error
```

However, if you just want to change it for the current session you can also do
that.

```erlang
1> logger:set_handler_config(default, formatter, {logger_formatter, #{}}).
ok
2> logger:error("Oh noes, another error").
1962-10-04T15:34:02.648713-04:00 error: Oh noes, another error
```

#### See also

- [logger_formatter's Configuration](`t:logger_formatter:config/0`)
- [Formatters](logger_chapter.md#formatters) in the Logging User's Guide
- `logger:set_handler_config/3`

### Add file and line number to log entries

You can change what is printed to the log by using the formatter template:

```erlang
$ cat sys.config
[{kernel,
  [{logger,
    [{handler, default, logger_std_h,
      #{ formatter => {logger_formatter,
        #{ template => [time," ", file,":",line," ",level,": ",msg,"\n"] }}}}]}]}].
$ erl -config sys
Eshell V10.5.1  (abort with ^G)
1> logger:error("Oh noes, more errors",#{ file => "shell.erl", line => 1 }).
1962-10-05T07:37:44.104241+02:00 shell.erl:1 error: Oh noes, more errors
```

Note that file and line have to be added in the metadata by the caller of
`logger:log/3` as otherwise Logger will not know from where it was called. The
file and line number are automatically added if you use the `?LOG_ERROR` macros
in `kernel/include/logger.hrl`.

#### See also

- [logger_formatter's Configuration](`t:logger_formatter:config/0`)
- [logger_formatter's Template](`t:logger_formatter:template/0`)
- [Logger Macros](`m:logger#module-macros`)
- [Metadata](logger_chapter.md#metadata) in the Logging User's Guide

## Configuring handlers

### Print logs to a file

Instead of printing the logs to stdout we print them to a rotating file log.

```erlang
$ cat sys.config
[{kernel,
  [{logger,
    [{handler, default, logger_std_h,
      #{ config => #{ file => "log/erlang.log",
                      max_no_bytes => 4096,
                      max_no_files => 5},
         formatter => {logger_formatter, #{}}}}]}]}].
$ erl -config sys
Eshell V10.5.1  (abort with ^G)
1> logger:error("Oh noes, even more errors").
ok
2> erlang:halt().
$ cat log/erlang.log
2019-10-07T11:47:16.837958+02:00 error: Oh noes, even more errors
```

#### See also

- `m:logger_std_h`
- [Handlers](logger_chapter.md#handlers) in the Logging User's Guide

### Debug only handler

Add a handler that prints `debug` log events to a file, while the default
handler prints only up to `notice` level events to standard out.

```erlang
$ cat sys.config
[{kernel,
  [{logger_level, all},
   {logger,
    [{handler, default, logger_std_h,
      #{ level => notice }},
     {handler, debug, logger_std_h,
      #{ filters => [{debug,{fun logger_filters:level/2, {stop, neq, debug}}}],
         config => #{ file => "log/debug.log" } }}
    ]}]}].
$ erl -config sys
Eshell V10.5.1  (abort with ^G)
1> logger:error("Oh noes, even more errors").
=ERROR REPORT==== 9-Oct-2019::14:40:54.784162 ===
Oh noes, even more errors
ok
2> logger:debug("A debug event").
ok
3> erlang:halt().
$ cat log/debug.log
2019-10-09T14:41:03.680541+02:00 debug: A debug event
```

In the configuration above we first raise the primary log level to max in order
for the debug log events to get to the handlers. Then we configure the default
handler to only log notice and below events, the default log level for a handler
is `all`. Then the debug handler is configured with a filter to stop any log
message that is not a debug level message.

It is also possible to do the same changes in an already running system using
the `logger` module. Then you do like this:

```erlang
$ erl
1> logger:set_handler_config(default, level, notice).
ok
2> logger:add_handler(debug, logger_std_h, #{
  filters => [{debug,{fun logger_filters:level/2, {stop, neq, debug}}}],
  config => #{ file => "log/debug.log" } }).
ok
3> logger:set_primary_config(level, all).
ok
```

It is important that you do not raise the primary log level before adjusting the
default handler's level as otherwise your standard out may be flooded by debug
log messages.

#### See also

- `m:logger_std_h`
- [Filters](logger_chapter.md#filters) in the Logging User's Guide

## Logging

### What to log and how

The simplest way to log something is by using the Logger macros and give a
report to the macro. For example if you want to log an error:

```erlang
?LOG_ERROR(#{ what => http_error, status => 418, src => ClientIP, dst => ServerIP }).
```

This will print the following in the default log:

```text
=ERROR REPORT==== 10-Oct-2019::12:13:10.089073 ===
    dst: {8,8,4,4}
    src: {8,8,8,8}
    status: 418
    what: http_error
```

or the below if you use a single line formatter:

```text
2019-10-10T12:14:11.921843+02:00 error: dst: {8,8,4,4}, src: {8,8,8,8}, status: 418, what: http_error
```

#### See also

- [Log Message](logger_chapter.md#log_message) in the Logging User's Guide

### Report call-backs and printing of events

If you want to do structured logging, but still want to have some control of how
the final log message is formatted you can give a `report_cb` as part of the
metadata with your log event.

```erlang
ReportCB = fun(#{ what := What, status := Status, src := Src, dst := Dst }) ->
                   {ok, #hostent{ h_name = SrcName }} = inet:gethostbyaddr(Src),
                   {ok, #hostent{ h_name = DstName }} = inet:gethostbyaddr(Dst),
                   {"What: ~p~nStatus: ~p~nSrc: ~s (~s)~nDst: ~s (~s)~n",
                    [What, Status, inet:ntoa(Src), SrcName, inet:ntoa(Dst), DstName]}
           end,
?LOG_ERROR(#{ what => http_error, status => 418, src => ClientIP, dst => ServerIP },
           #{ report_cb => ReportCB }).
```

This will print the following:

```text
=ERROR REPORT==== 10-Oct-2019::13:29:02.230863 ===
What: http_error
Status: 418
Src: 8.8.8.8 (dns.google)
Dst: 192.121.151.106 (erlang.org)
```

Note that the order that things are printed have changed, and also I added a
reverse-dns lookup of the IP address. This will not print as nicely when using a
single line formatter, however you can also use a report_cb fun with 2 arguments
where the second argument is the formatting options.

#### See also

- [Log Message](logger_chapter.md#log_message) in the Logging User's Guide
- [Logger Report Callbacks](`t:logger:report_cb/0`)

## Filters

Filters are used to remove or change log events before they reach the handlers.

### Process filters

If we only want debug messages from a specific process it is possible to do this
with a filter like this:

```erlang
%% Initial setup to use a filter for the level filter instead of the primary level
PrimaryLevel = maps:get(level, logger:get_primary_config()),
ok = logger:add_primary_filter(primary_level,
    {fun logger_filters:level/2, {log, gteq, PrimaryLevel}}),
logger:set_primary_config(filter_default, stop),
logger:set_primary_config(level, all),

%% Test that things work as they should
logger:notice("Notice should be logged"),
logger:debug("Should not be logged"),

%% Add the filter to allow PidToLog to send debug events
PidToLog = self(),
PidFilter = fun(LogEvent, _) when PidToLog =:= self() -> LogEvent;
               (_LogEvent, _) -> ignore end,
ok = logger:add_primary_filter(pid, {PidFilter,[]}),
logger:debug("Debug should be logged").
```

There is a bit of setup needed to allow filters to decide whether a specific
process should be allowed to log. This is because the default primary log level
is notice and it is enforced before the primary filters. So in order for the pid
filter to be useful we have to raise the primary log level to `all` and then add
a level filter that only lets certain messages at or greater than notice
through. When the setup is done, it is simple to add a filter that allows a
certain pid through.

Note that doing the primary log level filtering through a filter and not through
the level is quite a lot more expensive, so make sure to test that your system
can handle the extra load before you enable it on a production node.

#### See also

- [Filters](logger_chapter.md#filters) in the Logging User's Guide
- `logger_filters:level/2`
- `logger:set_primary_config/2`

### Domains

Domains are used to specify which subsystem a certain log event originates from.
The default handler will by default only log events with the domain `[otp]` or
without a domain. If you would like to include SSL log events into the default
handler log you could do this:

```erlang
1> logger:add_handler_filter(default,ssl_domain,
  {fun logger_filters:domain/2,{log,sub,[otp,ssl]}}).
2> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
3> ssl:connect("www.erlang.org",443,[{log_level,debug}]).
%% lots of text
```

#### See also

- [Filters](logger_chapter.md#filters) in the Logging User's Guide
- `logger_filters:domain/2`
- `logger:set_primary_config/2`
