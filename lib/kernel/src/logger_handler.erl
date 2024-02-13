%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2024. All Rights Reserved.
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
-module(logger_handler).
-moduledoc """
logger_handler behavior module.

The behaviour module for logger handlers. A logger handler is a callback module
that is called when a log event has passed all filters and is ready to be logged
somewhere. For more information see [Handlers](logger_chapter.md#handlers) in
the Users Guide.

## See Also

`m:logger_filters`, `m:logger_formatter`, `m:logger`
""".
-moduledoc(#{since => "OTP 27.0"}).

%%%
%%% Types
-doc """
Handler configuration data for Logger. The following default values apply:

- `level => all`
- `filter_default => log`
- `filters => []`
- `formatter => {logger_formatter, DefaultFormatterConfig`\}

In addition to these, the following fields are automatically inserted by Logger,
values taken from the two first parameters to `logger:add_handler/3`:

- `id => HandlerId`
- `module => Module`

These are read-only and cannot be changed in runtime.

Handler specific configuration data is inserted by the handler callback itself,
in a sub structure associated with the field named `config`. See the
`m:logger_std_h` and `m:logger_disk_log_h` manual pages for information about
the specific configuration for these handlers.

See the [`logger_formatter`](`t:logger_formatter:config/0`) manual page for
information about the default configuration for this formatter.
""".
-type config() :: #{id => id(),
                    config => term(),
                    level => logger:level() | all | none,
                    module => module(),
                    filter_default => log | stop,
                    filters => [{logger:filter_id(),logger:filter()}],
                    formatter => {module(),logger:formatter_config()}}.
-doc "A unique identifier for a handler instance.".
-type id() :: atom().

-export_type([config/0, id/0]).

%%%-----------------------------------------------------------------
%%% Callbacks
-doc """
This callback function is optional.

The function is called on a temporary process when a new handler is about to be
added. The purpose is to verify the configuration and initiate all resources
needed by the handler.

The handler identity is associated with the `id` key in `Config1`.

If everything succeeds, the callback function can add possible default values or
internal state values to the configuration, and return the adjusted map in
`{ok,Config2}`.

If the configuration is faulty, or if the initiation fails, the callback
function must return `{error,Reason}`.
""".
-doc(#{since => <<"OTP 21.0">>}).
-callback adding_handler(Config1) -> {ok, Config2} | {error, Reason} when
      Config1 :: config(),
      Config2 :: config(),
      Reason :: term().

-doc """
This callback function is optional.

The function is called on a temporary process when the configuration for a
handler is about to change. The purpose is to verify and act on the new
configuration.

`OldConfig` is the existing configuration and `NewConfig` is the new
configuration.

The handler identity is associated with the `id` key in `OldConfig`.

`SetOrUpdate` has the value `set` if the configuration change originates from a
call to [`logger:set_handler_config/2,3`](`logger:set_handler_config/2`), and
`update` if it originates from
[`logger:update_handler_config/2,3`](`logger:update_handler_config/2`). The
handler can use this parameter to decide how to update the value of the `config`
field, that is, the handler specific configuration data. Typically, if
`SetOrUpdate` equals `set`, values that are not specified must be given their
default values. If `SetOrUpdate` equals `update`, the values found in
`OldConfig` must be used instead.

If everything succeeds, the callback function must return a possibly adjusted
configuration in `{ok,Config}`.

If the configuration is faulty, the callback function must return
`{error,Reason}`.
""".
-doc(#{since => <<"OTP 21.2">>}).
-callback changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    {ok, Config} | {error, Reason} when
      SetOrUpdate :: set | update,
      OldConfig :: config(),
      NewConfig :: config(),
      Config :: config(),
      Reason :: term().

-doc """
This callback function is optional.

The function is called when one of the Logger API functions for fetching the
handler configuration is called, for example `logger:get_handler_config/1`.

It allows the handler to remove internal data fields from its configuration data
before it is returned to the caller.
""".
-doc(#{since => <<"OTP 21.2">>}).
-callback filter_config(Config) -> FilteredConfig when
      Config :: config(),
      FilteredConfig :: config().

-doc """
This callback function is mandatory.

The function is called when all primary filters and all handler filters for the
handler in question have passed for the given log event. It is called on the
client process, that is, the process that issued the log event.

The handler identity is associated with the `id` key in `Config`.

The handler must log the event.

The return value from this function is ignored by Logger.
""".
-doc(#{since => <<"OTP 21.0">>}).
-callback log(LogEvent, Config) -> term() when
      LogEvent :: logger:log_event(), Config :: config().

-doc """
This callback function is optional.

The function is called on a temporary process when a handler is about to be
removed. The purpose is to release all resources used by the handler.

The handler identity is associated with the `id` key in `Config`.

The return value is ignored by Logger.
""".
-doc(#{since => <<"OTP 21.0">>}).
-callback removing_handler(Config) -> ok when
      Config :: config().

-optional_callbacks([adding_handler/1, changing_config/3,
                     filter_config/1, removing_handler/1]).
