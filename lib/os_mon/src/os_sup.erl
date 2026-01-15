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
-module(os_sup).
-moduledoc """
Interface to OS System Messages

`os_sup` is a process providing a message passing service from the operating
system to the error logger in the Erlang runtime system. It is part of the
OS_Mon application, see [os_mon](os_mon_app.md). Available for Solaris and
Windows.

Messages received from the operating system results in an user defined callback
function being called. This function can do whatever filtering and formatting is
necessary and then deploy any type of logging suitable for the user's
application.

## Solaris Operation

The Solaris (SunOS 5.x) messages are retrieved from the syslog daemon,
`syslogd`.

Enabling the service includes actions which require root privileges, such as
change of ownership and file privileges of an executable binary file, and
creating a modified copy of the configuration file for `syslogd`. When `os_sup`
is terminated, the service must be disabled, meaning the original configuration
must be restored. Enabling/disabling can be done either outside or inside
`os_sup`. See [Configuration](`m:os_sup#config`) below.

> #### Warning {: .warning }
>
> This process cannot run in multiple instances on the same hardware. OS_Mon
> must be configured to start `os_sup` on one node only if two or more Erlang
> nodes execute on the same machine.

The format of received events is not defined.

## Windows Operation

The Windows messages are retrieved from the eventlog file.

The `nteventlog` module is used to implement `os_sup`. See `m:nteventlog`. Note
that the start functions of `nteventlog` does not need to be used, as in this
case the process is started automatically as part of the OS_Mon supervision
tree.

OS messages are formatted as a tuple
`{Time, Category, Facility, Severity, Message}`:

- **`Time = {MegaSecs, Secs, MicroSecs}`** - A time stamp as returned by the BIF
  `now/0`.

- **`Category = string()`** - Usually one of `"System"`, `"Application"` or
  `"Security"`. Note that the NT eventlog viewer has another notion of category,
  which in most cases is totally meaningless and therefore not imported into
  Erlang. What is called a category here is one of the main three types of
  events occurring in a normal NT system.

- **`Facility = string()`** - The source of the message, usually the name of the
  application that generated it. This could be almost any string. When matching
  messages from certain applications, the version number of the application may
  have to be accounted for. This is what the NT event viewer calls "source".

- **`Severity = string()`** - One of `"Error"`, `"Warning"`, `"Informational"`,
  `"Audit_Success"`, `"Audit_Faulure"` or, in case of a currently unknown
  Windows NT version `"Severity_Unknown"`.

- **`Message = string()`** - Formatted exactly as it would be in the NT eventlog
  viewer. Binary data is not imported into Erlang.

[](){: #config }

## Configuration

- **`os_sup_mfa = {Module, Function, Args}`** - The callback function to use.
  `Module` and `Function` are atoms and `Args` is a list of terms. When an OS
  message `Msg` is received, this function is called as
  [`apply(Module, Function, [Msg | Args])`](`apply/3`).

  Default is `{os_sup, error_report, [Tag]}` which will send the event to the
  error logger using
  [error_logger:error_report(Tag, Msg)](`error_logger:error_report/2`). `Tag` is
  the value of `os_sup_errortag`, see below.

- **`os_sup_errortag = atom()`** - This parameter defines the error report type
  used when messages are sent to error logger using the default callback
  function. Default is `std_error`, which means the events are handled by the
  standard event handler.

- **`os_sup_enable = bool()`** - Solaris only. Defines if the service should be
  enabled (and disabled) inside (`true`) or outside (`false`) `os_sup`. For
  backwards compatibility reasons, the default is `true`. The recommended value
  is `false`, as the Erlang emulator should normally not be run with `root`
  privileges, as is required for enabling the service.

- **`os_sup_own = string()`** - Solaris only. Defines the directory which
  contains the backup copy and the Erlang specific configuration files for
  `syslogd`, and a named pipe to receive the messages from `syslogd`. Default is
  `"/etc"`.

- **`os_sup_syslogconf = string()`** - Solaris only. Defines the full name of
  the configuration file for `syslogd`. Default is `"/etc/syslog.conf"`.

## See also

`m:error_logger`, [os_mon](os_mon_app.md)

`syslogd(1M)`, `syslog.conf(4)` in the Solaris documentation.
""".
-behaviour(gen_server).

%% API
-export([start_link/1, start/0, stop/0]).
-export([error_report/2]).
-export([enable/0, enable/2, disable/0, disable/2]).
-export([param_type/2, param_default/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).

-record(state, {port, mfa, config, path, conf}).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-doc false.
start_link({win32, _OSname}) ->
    Identifier = os_sup,
    MFA = os_mon:get_env(os_sup, os_sup_mfa),
    gen_server:start_link({local, os_sup_server}, nteventlog,
			  [Identifier, MFA], []);
start_link(_OS) ->
    gen_server:start_link({local, os_sup_server}, os_sup, [], []).

-doc false.
start() -> % for testing
    gen_server:start({local, os_sup_server}, os_sup, [], []).

-doc false.
stop() ->
    gen_server:call(os_sup_server, stop).

-doc false.
error_report(LogData, Tag) ->
    error_logger:error_report(Tag, LogData).

-doc(#{equiv => enable("/etc","/etc/syslog.conf")}).
-spec enable() -> ok | {error, Res} when
      Res :: string().
enable() ->
    command(enable).
-doc """
Enables the `os_sup` service. Needed on Solaris only.

If the configuration parameter `os_sup_enable` is `false`, this function is
called automatically by `os_sup`, using the values of `os_sup_own` and
`os_sup_syslogconf` as arguments.

If `os_sup_enable` is `true`, this function must be called _before_
OS_Mon/`os_sup` is started. `Dir` defines the directory which contains the
backup copy and the Erlang specific configuration files for `syslogd`, and a
named pipe to receive the messages from `syslogd`. Defaults to `"/etc"`. `Conf`
defines the full name of the configuration file for `syslogd`. Default is
`"/etc/syslog.conf"`.

Results in a OS call to:

```text
<PRIVDIR>/bin/mod_syslog otp Dir Conf
```

where `<PRIVDIR>` is the `priv` directory of OS_Mon, `code:priv_dir(os_mon)`.

Returns `ok` if this yields the expected result `"0"`, and `{error, Res}` if it
yields anything else.

> #### Note {: .info }
>
> This function requires root privileges to succeed.
""".
-spec enable(Dir, Conf) -> ok | {error, Res} when
      Dir :: string(),
      Conf :: string(),
      Res :: string().
enable(Path, Conf) ->
    command(enable, Path, Conf).

-doc(#{equiv => disable("/etc","/etc/syslog.conf")}).
-spec disable() -> ok | {error, Res} when
      Res :: string().
disable() ->
    command(disable).
-doc """
Disables the `os_sup` service. Needed on Solaris only.

If the configuration parameter `os_sup_enable` is `false`, this function is
called automatically by `os_sup`, using the same arguments as when
[`enable/2`](`enable/2`) was called.

If `os_sup_enable` is `true`, this function must be called _after_
OS_Mon/`os_sup` is stopped. `Dir` defines the directory which contains the
backup copy and the Erlang specific configuration files for `syslogd`, and a
named pipe to receive the messages from `syslogd`. Defaults to `"/etc"`. `Conf`
defines the full name of the configuration file for `syslogd`. Default is
`"/etc/syslog.conf"`.

Results in a OS call to:

```text
<PRIVDIR>/bin/mod_syslog nootp Dir Conf
```

where `<PRIVDIR>` is the `priv` directory of OS_Mon, `code:priv_dir(os_mon)`.

Returns `ok` if this yields the expected result `"0"`, and `{error, Res}` if it
yields anything else.

> #### Note {: .info }
>
> This function requires root privileges to succeed.
""".
-spec disable(Dir, Conf) -> ok | {error, Res} when
      Dir :: string(),
      Conf :: string(),
      Res :: string().
disable(Path, Conf) ->
    command(disable, Path, Conf).

-doc false.
param_type(os_sup_errortag, Val) when is_atom(Val) -> true;
param_type(os_sup_own, Val) -> io_lib:printable_list(Val);
param_type(os_sup_syslogconf, Val) -> io_lib:printable_list(Val);
param_type(os_sup_enable, Val) when Val==true; Val==false -> true;
param_type(os_sup_mfa, {Mod,Func,Args}) when is_atom(Mod),
					     is_atom(Func),
					     is_list(Args) -> true;
param_type(_Param, _Val) -> false.

-doc false.
param_default(os_sup_errortag) -> std_error;
param_default(os_sup_own) -> "/etc";
param_default(os_sup_syslogconf) -> "/etc/syslog.conf";
param_default(os_sup_enable) -> true;
param_default(os_sup_mfa) -> {os_sup, error_report, [std_error]}.

%%----------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------

-doc false.
init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),

    case os:type() of
	{unix, sunos} ->
	    init2();
	OS -> {stop, {unsupported_os, OS}}
    end.

init2() -> % Enable service if configured to do so
    ConfigP = os_mon:get_env(os_sup, os_sup_enable),
    case ConfigP of
	true -> % ..yes -- do enable
	    Path = os_mon:get_env(os_sup, os_sup_own),
	    Conf = os_mon:get_env(os_sup, os_sup_syslogconf),
	    case enable(Path, Conf) of
		ok ->
		    init3(#state{config=ConfigP, path=Path, conf=Conf});
		{error, Error} ->
		    {stop, {mod_syslog, Error}}
	    end;
	false -> % ..no -- skip directly to init3/1
	    init3(#state{config=ConfigP})
    end.

init3(State0) ->
    Port = start_portprogram(),

    %% Read the values of some configuration parameters
    MFA = case os_mon:get_env(os_sup, os_sup_mfa) of
	      {os_sup, error_report, _} ->
		  Tag = os_mon:get_env(os_sup, os_sup_errortag),
		  {os_sup, error_report, [Tag]};
	      MFA0 ->
		  MFA0
	  end,

    {ok, State0#state{port=Port, mfa=MFA}}.

-doc false.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc false.
handle_info({_Port, {data, Data}}, #state{mfa={M,F,A}} = State) ->
    apply(M, F, [Data | A]),
    {noreply, State};
handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_died, Reason}, State#state{port=not_used}};
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
terminate(_Reason, #state{port=Port} = State) ->
    case State#state.config of
	true when is_port(Port) ->
	    Port ! {self(), {command, "only_stdin"}},
	    Res = disable(State#state.path, State#state.conf),
	    port_close(Port),
	    if
		Res/="0" -> exit({mod_syslog, Res});
		true -> ok
	    end;
	true ->
	    Res = disable(State#state.path, State#state.conf),
	    if
		Res/="0" -> exit({mod_syslog, Res});
		true -> ok
	    end;
	false when is_port(Port) ->
	    Port ! {self(), {command, "only_stdin"}},
	    port_close(Port);
	false ->
	    ok
    end.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

start_portprogram() ->
    OwnPath = os_mon:get_env(os_sup, os_sup_own),
    Command =
	"\"" ++ filename:join([code:priv_dir(os_mon), "bin", "ferrule"]) ++
	"\" " ++ OwnPath,
    open_port({spawn, Command}, [{packet, 2}]).

%% os:cmd(cmd_str(enable)) should be done BEFORE starting os_sup
%% os:cmd(cmd_str(disable)) should be done AFTER os_sup is terminated
%% Both commands return "0" if successful
command(Mode) ->
    command(Mode, "/etc", "/etc/syslog.conf").
command(Mode, Path, Conf) ->
    case os:cmd(cmd_str(Mode, Path, Conf)) of
	"0" ->
	    ok;
	Error ->
	    {error, Error}
    end.

cmd_str(Mode, Path, Conf) ->
    %% modpgm modesw ownpath syslogconf
    PrivDir = code:priv_dir(os_mon),
    ModeSw =
	case Mode of
	    enable ->
		" otp ";
	    disable ->
		" nootp "
	end,
    PrivDir ++ "/bin/mod_syslog" ++ ModeSw ++ Path ++ " " ++ Conf.
