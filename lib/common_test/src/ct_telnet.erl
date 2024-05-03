%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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

-module(ct_telnet).
-moduledoc """
Common Test specific layer on top of Telnet client ct_telnet_client.erl

`Common Test` specific layer on top of Telnet client `ct_telnet_client.erl`.

Use this module to set up Telnet connections, send commands, and perform string
matching on the result. For information about how to use `ct_telnet` and
configure connections, specifically for UNIX hosts, see the `m:unix_telnet`
manual page.

Default values defined in `ct_telnet`:

[](){: #Default_values }

- Connection timeout (time to wait for connection) = 10 seconds
- Command timeout (time to wait for a command to return) = 10 seconds
- Max number of reconnection attempts = 3
- Reconnection interval (time to wait in between reconnection attempts) = 5
  seconds
- Keep alive (sends NOP to the server every 8 sec if connection is idle) =
  `true`
- Polling limit (max number of times to poll to get a remaining string
  terminated) = 0
- Polling interval (sleep time between polls) = 1 second
- The TCP_NODELAY option for the telnet socket is disabled (set to `false`) per
  default

These parameters can be modified by the user with the following configuration
term:

```erlang
 {telnet_settings, [{connect_timeout,Millisec},
                    {command_timeout,Millisec},
                    {reconnection_attempts,N},
                    {reconnection_interval,Millisec},
                    {keep_alive,Bool},
                    {poll_limit,N},
                    {poll_interval,Millisec},
                    {tcp_nodelay,Bool}]}.
```

`Millisec = integer(), N = integer()`

Enter the `telnet_settings` term in a configuration file included in the test
and `ct_telnet` retrieves the information automatically.

`keep_alive` can be specified per connection, if necessary. For details, see
`m:unix_telnet`.

## Logging

[](){: #Logging }

The default logging behavior of `ct_telnet` is to print information about
performed operations, commands, and their corresponding results to the test case
HTML log. The following is not printed to the HTML log: text strings sent from
the Telnet server that are not explicitly received by a `ct_telnet` function,
such as [`expect/3`](`expect/3`). However, `ct_telnet` can be configured to use
a special purpose event handler, implemented in `ct_conn_log_h`, for logging
_all_ Telnet traffic. To use this handler, install a `Common Test` hook named
`cth_conn_log`. Example (using the test suite information function):

```erlang
 suite() ->
     [{ct_hooks, [{cth_conn_log, [{conn_mod(),hook_options()}]}]}].
```

`conn_mod()` is the name of the `Common Test` module implementing the connection
protocol, that is, `ct_telnet`.

The `cth_conn_log` hook performs unformatted logging of Telnet data to a
separate text file. All Telnet communication is captured and printed, including
any data sent from the server. The link to this text file is located at the top
of the test case HTML log.

By default, data for all Telnet connections is logged in one common file (named
`default`), which can get messy, for example, if multiple Telnet sessions are
running in parallel. Therefore a separate log file can be created for each
connection. To configure this, use hook option `hosts` and list the names of the
servers/connections to be used in the suite. The connections must be named for
this to work (see [`ct_telnet:open/1,2,3,4`](`open/1`)).

Hook option `log_type` can be used to change the `cth_conn_log` behavior. The
default value of this option is `raw`, which results in the behavior described
above. If the value is set to `html`, all Telnet communication is printed to the
test case HTML log instead.

All `cth_conn_log` hook options described can also be specified in a
configuration file with configuration variable `ct_conn_log`.

_Example:_

```erlang
 {ct_conn_log, [{ct_telnet,[{log_type,raw},
                            {hosts,[key_or_name()]}]}]}
```

> #### Note {: .info }
>
> Hook options specified in a configuration file overwrite any hard-coded hook
> options in the test suite.

[](){: #Logging_example }

_Logging Example:_

The following `ct_hooks` statement causes printing of Telnet traffic to separate
logs for the connections `server1` and `server2`. Traffic for any other
connections is logged in the default Telnet log.

```erlang
 suite() ->
     [{ct_hooks,
       [{cth_conn_log, [{ct_telnet,[{hosts,[server1,server2]}]}]}]}].
```

As previously explained, this specification can also be provided by an entry
like the following in a configuration file:

```erlang
 {ct_conn_log, [{ct_telnet,[{hosts,[server1,server2]}]}]}.
```

In this case the `ct_hooks` statement in the test suite can look as follows:

```erlang
 suite() ->
     [{ct_hooks, [{cth_conn_log, []}]}].
```

## See Also

`m:unix_telnet`
""".

-export([open/1, open/2, open/3, open/4, close/1]).
-export([cmd/2, cmd/3, cmdf/3, cmdf/4, get_data/1, 
	 send/2, send/3, sendf/3, sendf/4,
	 expect/2, expect/3]).

%% Callbacks
-export([init/3,handle_msg/2,reconnect/2,terminate/2]).

%% Tool internals
-export([silent_teln_expect/6, teln_receive_until_prompt/3,
	 format_data/2]).
-export([start_gen_log/1, end_gen_log/0, log/3, log/4]).

-define(RECONNS,3).
-define(RECONN_TIMEOUT,5000).
-define(DEFAULT_TIMEOUT,10000).
-define(DEFAULT_PORT,23).
-define(POLL_LIMIT,0).
-define(POLL_INTERVAL,1000).

-include("ct_util.hrl").

-doc "For `target_name()`, see module `m:ct`.".
-type connection() :: handle() | {ct:target_name(), connection_type()} | ct:target_name().
-type connection_type() :: telnet | ts1 | ts2.
-doc "Handle for a specific Telnet connection, see module `m:ct`.".
-type handle() :: ct:handle().
-doc """
Regular expression matching all possible prompts for a specific target type.
`regexp` must not have any groups, that is, when matching, `re:run/3` (in
STDLIB) must return a list with one single element.
""".
-type prompt_regexp() :: string().
-export_type([connection/0, connection_type/0, handle/0, prompt_regexp/0]).

-record(state,{host,
	       port,
	       teln_pid,
	       prx,
	       buffer=[],
	       prompt=false,
	       name,
	       type,
	       target_mod,
	       keep_alive,
	       poll_limit=?POLL_LIMIT,
	       poll_interval=?POLL_INTERVAL,
	       extra,
	       conn_to=?DEFAULT_TIMEOUT, 
	       com_to=?DEFAULT_TIMEOUT, 
	       reconns=?RECONNS,
	       reconn_int=?RECONN_TIMEOUT,
	       tcp_nodelay=false}).

-doc """
open(Name) -> {ok, Handle} | {error, Reason}

Equivalent to [`ct_telnet:open(Name, telnet)`](`open/2`).
""".
open(Name) ->
    open(Name,telnet).

-doc """
open(Name, ConnType) -> {ok, Handle} | {error, Reason}

Opens a Telnet connection to the specified target host.
""".
open(Name,ConnType) ->
    case ct_util:get_key_from_name(Name) of
	{ok, unix} -> % unix host
	    open(Name, ConnType, unix_telnet, Name);
	{ok, Key} -> % any other, e.g. interwatch (iw), etc.
	    open(Name, ConnType, Key, Name);
	Error ->
	    Error
    end.

-doc """
open(KeyOrName, ConnType, TargetMod) -> {ok, Handle} | {error, Reason}

Equivalent to
[`ct_telnet:ct_telnet:open(KeyOrName, ConnType, TargetMod, [])`](`open/4`).
""".
open(KeyOrName,ConnType,TargetMod) ->
    open(KeyOrName,ConnType,TargetMod,KeyOrName).

-doc """
open(KeyOrName, ConnType, TargetMod, Extra) -> {ok, Handle} | {error, Reason}

Opens a Telnet connection to the specified target host.

The target data must exist in a configuration file. The connection can be
associated with `Name` and/or the returned `Handle`. To allocate a name for the
target, use one of the following alternatives:

- `ct:require/2` in a test case
- A `require` statement in the suite information function (`suite/0`)
- A `require` statement in a test case information function

If you want the connection to be associated with `Handle` only (if you, for
example, need to open multiple connections to a host), use `Key`, the
configuration variable name, to specify the target. Notice that a connection
without an associated target name can only be closed with the `Handle` value.

`TargetMod` is a module that exports the functions
`connect(Ip, Port, KeepAlive, Extra)` and `get_prompt_regexp()` for the
specified `TargetType` (for example, `unix_telnet`).

For `target_name()`, see module `m:ct`.

See also `ct:require/2`.
""".
open(KeyOrName,ConnType,TargetMod,Extra) ->
    case ct:get_config({KeyOrName,ConnType}) of
	undefined ->
	    log(undefined,open,"Failed: ~tp",[{not_available,KeyOrName}]),
	    {error,{not_available,KeyOrName,ConnType}};
	Addr ->
	    Addr1 =
		case Addr of
		    {_IP,_Port} ->
			Addr;
		    IP ->
			case ct:get_config({KeyOrName,port}) of
			    undefined -> IP;
			    P -> {IP,P}
			end
		end,
	    KeepAlive =
		case ct:get_config({KeyOrName,keep_alive}) of
		    undefined -> 
			case ct:get_config({telnet_settings,keep_alive}) of
			    undefined -> true;
			    Bool -> Bool
			end;
		    Bool -> Bool
		end,
	    log(undefined,open,"Connecting to ~tp(~tp)",
		[KeyOrName,Addr1]),
	    Reconnect =
		case ct:get_config({telnet_settings,reconnection_attempts}) of
		    0 -> false;
		    _ -> true
		end,
	    ct_gen_conn:start(full_addr(Addr1,ConnType),
			      {TargetMod,KeepAlive,Extra},
			      ?MODULE, [{name,KeyOrName},
					{reconnect,Reconnect},
					{old,true}])
    end.

-doc """
close(Connection) -> ok | {error, Reason}

Closes the Telnet connection and stops the process managing it.

A connection can be associated with a target name and/or a handle. If
`Connection` has no associated target name, it can only be closed with the
handle value (see [`ct_telnet:open/4`](`open/4`)).
""".
close(Connection) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    log(undefined,close,"Connection closed, handle: ~w",[Pid]),
	    case ct_gen_conn:stop(Pid) of
		{error,{process_down,Pid,_}} ->
		    {error,already_closed};
		Result ->
		    Result
	    end;
	Error ->
	    Error
    end.

%%%=================================================================
%%% Test suite interface
%%%-----------------------------------------------------------------

-doc """
cmd(Connection, Cmd) -> {ok, Data} | {error, Reason}

Equivalent to [`ct_telnet:cmd(Connection, Cmd, [])`](`cmd/3`).
""".
cmd(Connection,Cmd) ->
    cmd(Connection,Cmd,[]).

-doc """
cmd(Connection, Cmd, Opts) -> {ok, Data} | {error, Reason}

Sends a command through Telnet and waits for prompt.

By default, this function adds "\\n" to the end of the specified command. If
this is not desired, use option `{newline,false}`. This is necessary, for
example, when sending Telnet command sequences prefixed with character Interpret
As Command (IAC). Option `{newline,string()}` can also be used if a different
line end than "\\n" is required, for instance `{newline,"\r\n"}`, to add both
carriage return and newline characters.

Option `timeout` specifies how long the client must wait for prompt. If the time
expires, the function returns `{error,timeout}`. For information about the
default value for the command timeout, see the
[list of default values](`m:ct_telnet#Default_values`) in the beginning of this
module.
""".
cmd(Connection,Cmd,Opts) when is_list(Opts) ->
    case check_cmd_opts(Opts) of
	ok ->
	    case get_handle(Connection) of
		{ok,Pid} ->
		    call(Pid,{cmd,Cmd,Opts});
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end;
cmd(Connection,Cmd,Timeout) when is_integer(Timeout); Timeout==default ->
    %% This clause is kept for backwards compatibility only
    cmd(Connection,Cmd,[{timeout,Timeout}]).

check_cmd_opts([{timeout,Timeout}|Opts]) when is_integer(Timeout);
					      Timeout==default ->
    check_cmd_opts(Opts);
check_cmd_opts([]) ->
    ok;
check_cmd_opts(Opts) ->
    check_send_opts(Opts).

-doc """
cmdf(Connection, CmdFormat, Args) -> {ok, Data} | {error, Reason}

Equivalent to [`ct_telnet:cmdf(Connection, CmdFormat, Args, [])`](`cmdf/4`).
""".
cmdf(Connection,CmdFormat,Args) ->
    cmdf(Connection,CmdFormat,Args,[]).

-doc """
cmdf(Connection, CmdFormat, Args, Opts) -> {ok, Data} | {error, Reason}

Sends a Telnet command and waits for prompt (uses a format string and a list of
arguments to build the command).

For details, see [`ct_telnet:cmd/3`](`cmd/3`).
""".
cmdf(Connection,CmdFormat,Args,Opts) when is_list(Args) ->
    Cmd = lists:flatten(io_lib:format(CmdFormat,Args)),
    cmd(Connection,Cmd,Opts).

-doc """
get_data(Connection) -> {ok, Data} | {error, Reason}

Gets all data received by the Telnet client since the last command was sent.
Only newline-terminated strings are returned. If the last received string has
not yet been terminated, the connection can be polled automatically until the
string is complete.

The polling feature is controlled by the configuration values `poll_limit` and
`poll_interval` and is by default disabled. This means that the function
immediately returns all complete strings received and saves a remaining
non-terminated string for a later `get_data` call.
""".
get_data(Connection) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,get_data);
	Error ->
	    Error
    end.

-doc """
send(Connection, Cmd) -> ok | {error, Reason}

Equivalent to [`ct_telnet:send(Connection, Cmd, [])`](`send/3`).
""".
send(Connection,Cmd) ->
    send(Connection,Cmd,[]).

-doc """
send(Connection, Cmd, Opts) -> ok | {error, Reason}

Sends a Telnet command and returns immediately.

By default, this function adds "\\n" to the end of the specified command. If
this is not desired, option `{newline,false}` can be used. This is necessary,
for example, when sending Telnet command sequences prefixed with character
Interpret As Command (IAC). Option `{newline,string()}` can also be used if a
different line end than "\\n" is required, for instance `{newline,"\r\n"}`, to
add both carriage return and newline characters.

The resulting output from the command can be read with
[`ct_telnet:get_data/2`](`get_data/1`) or [`ct_telnet:expect/2,3`](`expect/2`).
""".
-doc(#{since => <<"OTP 17.4">>}).
send(Connection,Cmd,Opts) ->
    case check_send_opts(Opts) of
	ok ->
	    case get_handle(Connection) of
		{ok,Pid} ->
		    call(Pid,{send,Cmd,Opts});
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

check_send_opts([{newline,Bool}|Opts]) when is_boolean(Bool) ->
    check_send_opts(Opts);
check_send_opts([{newline,String}|Opts]) when is_list(String) ->
    case lists:all(fun(I) when is_integer(I), I>=0, I=<127 -> true;
                      (_) -> false
                   end, String) of
        true ->
            check_send_opts(Opts);
        false ->
            {error,{invalid_option,{newline,String}}}
    end;
check_send_opts([Invalid|_]) ->
    {error,{invalid_option,Invalid}};
check_send_opts([]) ->
    ok.

-doc """
sendf(Connection, CmdFormat, Args) -> ok | {error, Reason}

Equivalent to [`ct_telnet:sendf(Connection, CmdFormat, Args, [])`](`sendf/4`).
""".
sendf(Connection,CmdFormat,Args) when is_list(Args) ->
    sendf(Connection,CmdFormat,Args,[]).

-doc """
sendf(Connection, CmdFormat, Args, Opts) -> ok | {error, Reason}

Sends a Telnet command and returns immediately (uses a format string and a list
of arguments to build the command).

For details, see [`ct_telnet:send/3`](`send/3`).
""".
-doc(#{since => <<"OTP 17.4">>}).
sendf(Connection,CmdFormat,Args,Opts) when is_list(Args) ->
    Cmd = lists:flatten(io_lib:format(CmdFormat,Args)),
    send(Connection,Cmd,Opts).

-doc """
expect(Connection, Patterns) -> term()

Equivalent to [`ct_telnet:expect(Connections, Patterns, [])`](`expect/3`).
""".
expect(Connection,Patterns) ->
    expect(Connection,Patterns,[]).

-doc """
expect(Connection, Patterns, Opts) -> {ok, Match} | {ok, MatchList, HaltReason}
| {error, Reason}

Gets data from Telnet and waits for the expected pattern.

`Pattern` can be a POSIX regular expression. The function returns when a pattern
is successfully matched (at least one, in the case of multiple patterns).

`RxMatch` is a list of matched strings. It looks as follows
`[FullMatch, SubMatch1, SubMatch2, ...]`, where `FullMatch` is the string
matched by the whole regular expression, and `SubMatchN` is the string that
matched subexpression number `N`. Subexpressions are denoted with `'(' ')'` in
the regular expression.

If a `Tag` is specified, the returned `Match` also includes the matched `Tag`.
Otherwise, only `RxMatch` is returned.

_Options:_

- **`idle_timeout`** - Indicates that the function must return if the Telnet
  client is idle (that is, if no data is received) for more than `IdleTimeout`
  milliseconds. Default time-out is 10 seconds.

- **`total_timeout`** - Sets a time limit for the complete `expect` operation.
  After `TotalTimeout` milliseconds, `{error,timeout}` is returned. Default is
  `infinity` (that is, no time limit).

- **`ignore_prompt | no_prompt_check`** - >The function returns when a prompt is
  received, even if no pattern has yet been matched, and
  `{error,{prompt,Prompt}}` is returned. However, this behavior can be modified
  with option `ignore_prompt` or option `no_prompt_check`, which tells `expect`
  to return only when a match is found or after a time-out.

- **`ignore_prompt`** - `ct_telnet` ignores any prompt found. This option is
  useful if data sent by the server can include a pattern matching prompt
  `regexp` (as returned by `TargedMod:get_prompt_regexp/0`), but is not to not
  cause the function to return.

- **`no_prompt_check`** - `ct_telnet` does not search for a prompt at all. This
  is useful if, for example, `Pattern` itself matches the prompt.

- **`wait_for_prompt`** - Forces `ct_telnet` to wait until the prompt string is
  received before returning (even if a pattern has already been matched). This
  is equal to calling
  [`expect(Conn, Patterns++[{prompt,Prompt}], [sequence|Opts])`](`expect/3`).
  Notice that option `idle_timeout` and `total_timeout` can abort the operation
  of waiting for prompt.

- **`repeat | repeat, N`** - The pattern(s) must be matched multiple times. If
  `N` is specified, the pattern(s) are matched `N` times, and the function
  returns `HaltReason = done`. This option can be interrupted by one or more
  `HaltPatterns`. `MatchList` is always returned, that is, a list of `Match`
  instead of only one `Match`. Also `HaltReason` is returned.

- **`sequence`** - All patterns must be matched in a sequence. A match is not
  concluded until all patterns are matched. This option can be interrupted by
  one or more `HaltPatterns`. `MatchList` is always returned, that is, a list of
  `Match` instead of only one `Match`. Also `HaltReason` is returned.

_Example 1:_

```erlang
 expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],[sequence,{halt,[{nnn,"NNN"}]}])
```

First this tries to match `"ABC"`, and then `"XYZ"`, but if `"NNN"` appears, the
function returns `{error,{nnn,["NNN"]}}`. If both `"ABC"` and `"XYZ"` are
matched, the function returns `{ok,[AbcMatch,XyzMatch]}`.

_Example 2:_

```erlang
 expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],[{repeat,2},{halt,[{nnn,"NNN"}]}])
```

This tries to match `"ABC"` or `"XYZ"` twice. If `"NNN"` appears, the function
returns `HaltReason = {nnn,["NNN"]}`.

Options `repeat` and `sequence` can be combined to match a sequence multiple
times.
""".
expect(Connection,Patterns,Opts) ->
    case get_handle(Connection) of
        {ok,Pid} ->
            case call(Pid,{expect,Patterns,Opts}) of
                {error,Reason} when element(1,Reason)==bad_pattern ->
                    %% Faulty user input - should fail the test case
                    exit({Reason,{?MODULE,?FUNCTION_NAME,3}});
                Other ->
                    Other
            end;
        Error ->
            Error
    end.

%%%=================================================================
%%% Callback functions

-doc false.
init(Name,{Ip,Port,Type},{TargetMod,KeepAlive,Extra}) ->
    S0 = case ct:get_config(telnet_settings) of
	     undefined ->
		 #state{};
	     Settings ->
		 set_telnet_defaults(Settings,#state{})				    
	 end,
    %% Handle old user versions of TargetMod
    _ = code:ensure_loaded(TargetMod),
    try
	case erlang:function_exported(TargetMod,connect,7) of
	    true ->
		TargetMod:connect(Name,Ip,Port,S0#state.conn_to,
				  KeepAlive,S0#state.tcp_nodelay,Extra);
	    false ->
		TargetMod:connect(Name,Ip,Port,S0#state.conn_to,
				  KeepAlive,Extra)
	end
    of
	{ok,TelnPid} ->
	    put({ct_telnet_pid2name,TelnPid},Name),
	    S1 = S0#state{host=Ip,
			  port=Port,
			  teln_pid=TelnPid,
			  name=Name,
			  type=type(Type),
			  target_mod=TargetMod,
			  keep_alive=KeepAlive,
			  extra=Extra,
			  prx=TargetMod:get_prompt_regexp()},
	    log(S1,open,
		"Opened telnet connection\n"
		"IP: ~p\n"
		"Port: ~p\n"
		"Command timeout: ~p\n"
		"Reconnection attempts: ~p\n"
		"Reconnection interval: ~p\n"
		"Connection timeout: ~p\n"
		"Keep alive: ~w\n"
		"Poll limit: ~w\n"
		"Poll interval: ~w\n"
		"TCP nodelay: ~w",
		[Ip,Port,S1#state.com_to,S1#state.reconns,
		 S1#state.reconn_int,S1#state.conn_to,KeepAlive,
		 S1#state.poll_limit,S1#state.poll_interval,
		 S1#state.tcp_nodelay]),
	    {ok,TelnPid,S1};
	Error ->
	    Error
    catch
	_:Reason ->
	    {error,Reason}
    end.

type(telnet) -> ip;
type(TS) when TS==ts1;TS==ts2 -> ts.

set_telnet_defaults([{connect_timeout,CnTo}|Ss],S) ->
    set_telnet_defaults(Ss,S#state{conn_to=CnTo});
set_telnet_defaults([{command_timeout,CmTo}|Ss],S) ->
    set_telnet_defaults(Ss,S#state{com_to=CmTo});
set_telnet_defaults([{reconnection_attempts,Rs}|Ss],S) ->
    set_telnet_defaults(Ss,S#state{reconns=Rs});
set_telnet_defaults([{reconnection_interval,RInt}|Ss],S) ->
    set_telnet_defaults(Ss,S#state{reconn_int=RInt});
set_telnet_defaults([{keep_alive,_}|Ss],S) ->
    set_telnet_defaults(Ss,S);
set_telnet_defaults([{poll_limit,PL}|Ss],S) ->
    set_telnet_defaults(Ss,S#state{poll_limit=PL});
set_telnet_defaults([{poll_interval,PI}|Ss],S) ->
    set_telnet_defaults(Ss,S#state{poll_interval=PI});
set_telnet_defaults([{tcp_nodelay,NoDelay}|Ss],S) ->
    set_telnet_defaults(Ss,S#state{tcp_nodelay=NoDelay});
set_telnet_defaults([Unknown|Ss],S) ->
    force_log(S,error,
	      "Bad element in telnet_settings: ~tp",[Unknown]),
    set_telnet_defaults(Ss,S);
set_telnet_defaults([],S) ->
    S.

-doc false.
handle_msg({cmd,Cmd,Opts},State) ->
    start_gen_log(heading(cmd,State#state.name)),
    log(State,cmd,"Cmd: ~tp",[Cmd]),

    %% whatever is in the buffer from previous operations
    %% will be ignored as we go ahead with this telnet cmd

    debug_cont_gen_log("Throwing Buffer:",[]),
    debug_log_lines(State#state.buffer),

    _ = case {State#state.type,State#state.prompt} of
	{ts,_} ->
	    silent_teln_expect(State#state.name,
			       State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{idle_timeout,2000}]);
	{ip,false} ->
	    silent_teln_expect(State#state.name,
			       State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{idle_timeout,200}]);
	{ip,true} ->
	    ok
    end,
    TO = case proplists:get_value(timeout,Opts,default) of
	     default -> State#state.com_to;
	     Timeout -> Timeout
	 end,
    Newline = proplists:get_value(newline,Opts,true),
    {Return,NewBuffer,Prompt} = 
	case teln_cmd(State#state.teln_pid, Cmd, State#state.prx,
		      Newline, TO) of
	    {ok,Data,_PromptType,Rest} ->
		log(State,recv,"Return: ~tp",[{ok,Data}]),
		{{ok,Data},Rest,true};
	    Error ->
		Retry = {retry,{Error,
				{State#state.name,
				 State#state.type},
				State#state.teln_pid,
				{cmd,Cmd,Opts}}},
		log(State,recv,"Return: ~tp",[Error]),
		{Retry,[],false}
	end,
    end_gen_log(),
    {Return,State#state{buffer=NewBuffer,prompt=Prompt}};
handle_msg({send,Cmd,Opts},State) ->
    start_gen_log(heading(send,State#state.name)),
    log(State,send,"Sending: ~tp",[Cmd]),
    
    debug_cont_gen_log("Throwing Buffer:",[]),
    debug_log_lines(State#state.buffer),
    
    _ = case {State#state.type,State#state.prompt} of
	{ts,_} -> 
	    silent_teln_expect(State#state.name,
			       State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{idle_timeout,2000}]);
	{ip,false} -> 
	    silent_teln_expect(State#state.name,
			       State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{idle_timeout,200}]);
	{ip,true} ->
	    ok
    end,
    Newline = proplists:get_value(newline,Opts,true),
    ct_telnet_client:send_data(State#state.teln_pid,Cmd,Newline),
    end_gen_log(),
    {ok,State#state{buffer=[],prompt=false}};
handle_msg(get_data,State) ->
    start_gen_log(heading(get_data,State#state.name)),
    log(State,cmd,"Reading data...",[]),
    {ok,Data,Buffer} = teln_get_all_data(State,State#state.buffer,[],[],
					 State#state.poll_limit),
    log(State,recv,"Return: ~tp",[{ok,Data}]),
    end_gen_log(),
    {{ok,Data},State#state{buffer=Buffer}};
handle_msg({expect,Pattern,Opts},State) ->
    start_gen_log(heading(expect,State#state.name)),
    log(State,expect,"Expect: ~tp\nOpts = ~tp\n",[Pattern,Opts]),
    {Return,NewBuffer,Prompt} = 
	case teln_expect(State#state.name,
			 State#state.teln_pid,
			 State#state.buffer,
			 Pattern,
			 State#state.prx,
			 Opts) of
	    {ok,Data,Rest} ->
		P = check_if_prompt_was_reached(Data,[]),
		{{ok,Data},Rest,P};
	    {ok,Data,HaltReason,Rest} ->
		force_log(State,expect,"HaltReason: ~tp",[HaltReason]),
		P = check_if_prompt_was_reached(Data,HaltReason),
		{{ok,Data,HaltReason},Rest,P};
	    {error,Reason,Rest} ->
		force_log(State,expect,"Expect failed\n~tp",[{error,Reason}]),
		P = check_if_prompt_was_reached([],Reason),
		{{error,Reason},Rest,P};
	    {error,Reason} ->
		force_log(State,expect,"Expect failed\n~tp",[{error,Reason}]),
		P = check_if_prompt_was_reached([],Reason),
		{{error,Reason},[],P}
	end,
    end_gen_log(),
    Return1 = case Return of
		  {error,_} -> {retry,{Return,
				       {State#state.name,
					State#state.type},
				       State#state.teln_pid,
				       {expect,Pattern,Opts}}};
		  _ -> Return
	      end,	    
    {Return1,State#state{buffer=NewBuffer,prompt=Prompt}}.


-doc false.
reconnect({Ip,Port,_Type},State) ->
    reconnect(Ip,Port,State#state.reconns,State).
reconnect(Ip,Port,N,State=#state{name=Name,
				 target_mod=TargetMod,
				 keep_alive=KeepAlive,
				 extra=Extra,
				 conn_to=ConnTo,
				 reconn_int=ReconnInt,
				 tcp_nodelay=NoDelay}) ->
    %% Handle old user versions of TargetMod
    ConnResult =
	case erlang:function_exported(TargetMod,connect,7) of
	    true ->
		TargetMod:connect(Name,Ip,Port,ConnTo,KeepAlive,NoDelay,Extra);
	    false ->
		TargetMod:connect(Name,Ip,Port,ConnTo,KeepAlive,Extra)
	end,
    case ConnResult of
	{ok,NewPid} ->
	    put({ct_telnet_pid2name,NewPid},Name),
	    {ok, NewPid, State#state{teln_pid=NewPid}};
	Error when N==0 ->
	    Error;
	_Error ->
	    log(State,reconnect,"Reconnect failed!","Retries left: ~w",[N]),
	    timer:sleep(ReconnInt),
	    reconnect(Ip,Port,N-1,State)
    end.


-doc false.
terminate(TelnPid,State) ->
    Result = ct_telnet_client:close(TelnPid),
    log(State,close,"Telnet connection for ~w closed.",[TelnPid]),
    Result.

%%%=================================================================
%%% Internal function
get_handle(Pid) when is_pid(Pid) ->
    {ok,Pid};
get_handle({Name,Type}) when Type==telnet;Type==ts1;Type==ts2 ->
    case ct_util:get_connection(Name,?MODULE) of
	{ok,Conn} ->
	    case get_handle(Type,Conn) of
		{ok,Pid} -> 
		    {ok,Pid};
		_Error ->
		    case ct_util:get_key_from_name(Name) of
			{ok,node} ->
			    open(Name,Type,ct_telnet_cello_node);
			{ok,unix} -> % unix host
			    open(Name,Type,unix_telnet,Name);
			{ok,Key} -> % any other, e.g. interwatch (iw)
			    open(Name,Type,Key,Name);
			Error ->
			    Error
		    end
	    end;
	Error ->
	    Error
    end;
get_handle(Name) ->
    get_handle({Name,telnet}).

get_handle(Type,{Pid,{_,_,Type}}) ->
    {ok,Pid};
get_handle(Type,_) ->
    {error,{no_such_connection,Type}}.

full_addr({Ip,Port},Type) ->
    {Ip,Port,Type};
full_addr(Ip,Type) ->
    {Ip,?DEFAULT_PORT,Type}.

call(Pid,Msg) ->
    ct_gen_conn:call(Pid,Msg).

check_if_prompt_was_reached({prompt,_},_) ->
    true;
check_if_prompt_was_reached(_,{prompt,_}) ->
    true;
check_if_prompt_was_reached(Data,_) when is_list(Data) ->
    lists:keymember(prompt,1,Data);
check_if_prompt_was_reached(_,_) ->
    false.

%%%-----------------------------------------------------------------
%%% Functions for logging ct_telnet reports and telnet data 

heading(Action,undefined) ->
    io_lib:format("~w ~w",[?MODULE,Action]);
heading(Action,Name) ->
    io_lib:format("~w ~w for ~tp",[?MODULE,Action,Name]).

force_log(State,Action,String,Args) ->
    log(State,Action,String,Args,true).

%%%-----------------------------------------------------------------
-doc false.
log(State,Action,String,Args) when is_record(State, state) ->
    log(State,Action,String,Args,false);
log(Name,Action,String,Args) when is_atom(Name) ->
    log(#state{name=Name},Action,String,Args,false);
log(TelnPid,Action,String,Args) when is_pid(TelnPid) ->
    log(#state{teln_pid=TelnPid},Action,String,Args,false).

%%%-----------------------------------------------------------------
-doc false.
log(undefined,String,Args) ->
    log(#state{},undefined,String,Args,false);
log(Name,String,Args) when is_atom(Name) ->
    log(#state{name=Name},undefined,String,Args,false);
log(TelnPid,String,Args) when is_pid(TelnPid) ->
    log(#state{teln_pid=TelnPid},undefined,String,Args).

%%%-----------------------------------------------------------------
log(#state{name=Name,teln_pid=TelnPid,host=Host,port=Port},
    Action,String,Args,ForcePrint) ->
    Name1 = if Name == undefined -> get({ct_telnet_pid2name,TelnPid});
	       true              -> Name
	    end,
    Silent = get(silent),

    if Action == general_io ->
	    case ct_util:get_testdata({cth_conn_log,?MODULE}) of
		HookMode when HookMode /= undefined, HookMode /= silent,
			      Silent /= true ->
		    error_logger:info_report(#conn_log{header=false,
						       client=self(),
						       conn_pid=TelnPid,
						       address={Host,Port},
						       name=Name1,
						       action=Action,
						       module=?MODULE},
					     {String,Args});
		_ -> %% hook inactive or silence requested
		    ok
	    end;

       true ->	    
	    if Action == open; Action == close; Action == reconnect;
	       Action == info; Action == error ->
		    ct_gen_conn:log(heading(Action,Name1),String,Args);

	       ForcePrint == false ->
		    case ct_util:is_silenced(telnet) of
			true  ->
			    ok;
			false ->
			    ct_gen_conn:cont_log_no_timestamp(String,Args)
		    end;
	       
	       ForcePrint == true ->
		    case ct_util:is_silenced(telnet) of
			true ->
			    %% call log/3 now instead of cont_log/2 since 
			    %% start_gen_log/1 will not have been previously
			    %% called
			    ct_gen_conn:log(heading(Action,Name1),String,Args);
			false ->
			    ct_gen_conn:cont_log_no_timestamp(String,Args)
		    end
	    end
    end.

%%%-----------------------------------------------------------------
-doc false.
start_gen_log(Heading) ->
    %% check if output is suppressed
    case ct_util:is_silenced(telnet) of
	true  -> ok;
	false -> ct_gen_conn:start_log(Heading)
    end.

%%%-----------------------------------------------------------------
-doc false.
end_gen_log() -> 
    %% check if output is suppressed
    case ct_util:is_silenced(telnet) of
	true  -> ok;
	false -> ct_gen_conn:end_log()
    end.

%% Debug printouts.
debug_cont_gen_log(Str,Args) ->
    Old = put(silent,true),
    ct_gen_conn:cont_log(Str,Args),
    put(silent,Old).

%% Log callback - called from the error handler process
-doc false.
format_data(_How,{String,Args}) ->
    io_lib:format(String,Args).

%%%=================================================================
%%% Abstraction layer on top of ct_telnet_client.erl
teln_cmd(Pid,Cmd,Prx,Newline,Timeout) ->
    ct_telnet_client:send_data(Pid,Cmd,Newline),
    teln_receive_until_prompt(Pid,Prx,Timeout).

teln_get_all_data(State=#state{teln_pid=Pid,prx=Prx},Data,Acc,LastLine,Polls) ->
    case check_for_prompt(Prx,LastLine++Data) of
	{prompt,Lines,_PromptType,Rest} ->
	    teln_get_all_data(State,Rest,[Lines|Acc],[],State#state.poll_limit);
	{noprompt,Lines,LastLine1} ->
	    case ct_telnet_client:get_data(Pid) of
		{ok,[]} when LastLine1 /= [], Polls > 0 ->
		    %% No more data from server but the last string is not
		    %% a complete line (maybe because of a slow connection),
		    timer:sleep(State#state.poll_interval),
		    NewPolls = if Polls == infinity -> infinity;
				  true              -> Polls-1
			       end,
		    teln_get_all_data(State,[],[Lines|Acc],LastLine1,NewPolls);
		{ok,[]} ->
		    {ok,lists:reverse(lists:append([Lines|Acc])),LastLine1};
		{ok,Data1} ->
		    teln_get_all_data(State,Data1,[Lines|Acc],LastLine1,
				      State#state.poll_limit)
	    end
    end.
    
%% Expect options record
-record(eo,{teln_pid,
	    prx,
	    idle_timeout,
	    total_timeout,
	    haltpatterns=[],
	    seq=false,
	    repeat=false,
	    found_prompt=false,
	    prompt_check=true}).

%% Externally the silent_teln_expect function shall only be used
%% by the TargetModule, i.e. the target specific module which
%% implements connect/2 and get_prompt_regexp/0.
-doc false.
silent_teln_expect(Name,Pid,Data,Pattern,Prx,Opts) ->
    Old = put(silent,true),
    Result = teln_expect(Name,Pid,Data,Pattern,Prx,Opts),
    put(silent,Old),
    Result.

%% teln_expect/6
%%
%% This function implements the expect functionality over telnet. In
%% general there are three possible ways to go:
%% 1) Single: One or more patterns are given, and the function return
%% when one of the patterns are matched.
%% 2) Sequence: Several patterns are given, and they are matched in
%% the order they appear in the pattern list.
%% 3a) Repeat (single): 1) is repeated either N times or until a halt
%% condition is fulfilled.
%% 3b) Repeat (sequence): 2) is repeated either N times or until a
%% halt condition is fulfilled.
teln_expect(Name,Pid,Data,Pattern0,Prx,Opts) ->
    HaltPatterns0 =
	case get_ignore_prompt(Opts) of
	    true ->
		get_haltpatterns(Opts);
	    false ->
		[prompt | get_haltpatterns(Opts)]
	end,
    case convert_pattern(HaltPatterns0,false) of
        {ok,HaltPatterns} ->
            {WaitForPrompt,Pattern1,Opts1} = wait_for_prompt(Pattern0,Opts),
            Seq = get_seq(Opts1),
            case convert_pattern(Pattern1,Seq) of
                {ok,Pattern2} ->
                    {IdleTimeout,TotalTimeout} = get_timeouts(Opts1),
                    PromptCheck = get_prompt_check(Opts1),

                    EO = #eo{teln_pid=Pid,
                             prx=Prx,
                             idle_timeout=IdleTimeout,
                             total_timeout=TotalTimeout,
                             seq=Seq,
                             haltpatterns=HaltPatterns,
                             prompt_check=PromptCheck},
    
                    case get_repeat(Opts1) of
                        false ->
                            case teln_expect1(Name,Pid,Data,Pattern2,[],EO) of
                                {ok,Matched,Rest} when WaitForPrompt ->
                                    case lists:reverse(Matched) of
                                        [{prompt,_},Matched1] ->
                                            {ok,Matched1,Rest};
                                        [{prompt,_}|Matched1] ->
                                            {ok,lists:reverse(Matched1),Rest}
                                    end;
                                {ok,Matched,Rest} ->
                                    {ok,Matched,Rest};
                                {halt,Why,Rest} ->
                                    {error,Why,Rest};
                                {error,Reason} ->
                                    {error,Reason}
                            end;
                        N ->
                            EO1 = EO#eo{repeat=N},
                            repeat_expect(Name,Pid,Data,Pattern2,[],EO1)
                    end;
               Error ->
                    Error
            end;
        Error ->
            Error
    end.

convert_pattern(Pattern0,Seq)
  when Pattern0==[] orelse (is_list(Pattern0) and not is_integer(hd(Pattern0))) ->
    Pattern =
        case Seq of
            true -> Pattern0;
            false -> rm_dupl(Pattern0,[])
        end,
    compile_pattern(Pattern,[]);
convert_pattern(Pattern,_Seq) ->
    compile_pattern([Pattern],[]).

rm_dupl([P|Ps],Acc) ->
    case lists:member(P,Acc) of
	true ->
	    rm_dupl(Ps,Acc);
	false ->
	    rm_dupl(Ps,[P|Acc])
    end;
rm_dupl([],Acc) ->
    lists:reverse(Acc).

compile_pattern([prompt|Patterns],Acc) ->
    compile_pattern(Patterns,[prompt|Acc]);
compile_pattern([{prompt,_}=P|Patterns],Acc) ->
    compile_pattern(Patterns,[P|Acc]);
compile_pattern([{Tag,Pattern}|Patterns],Acc) ->
    try re:compile(Pattern,[unicode]) of
        {ok,MP} -> compile_pattern(Patterns,[{Tag,MP}|Acc]);
        {error,Error} -> {error,{bad_pattern,{Tag,Pattern},Error}}
    catch error:badarg -> {error,{bad_pattern,{Tag,Pattern}}}
    end;
compile_pattern([Pattern|Patterns],Acc) ->
    try re:compile(Pattern,[unicode]) of
        {ok,MP} -> compile_pattern(Patterns,[MP|Acc]);
        {error,Error} -> {error,{bad_pattern,Pattern,Error}}
    catch error:badarg -> {error,{bad_pattern,Pattern}}
    end;
compile_pattern([],Acc) ->
    {ok,lists:reverse(Acc)}.

get_timeouts(Opts) ->
    {case lists:keysearch(idle_timeout,1,Opts) of
	 {value,{_,T}} ->
	     T;
	 false ->
	     %% this check is for backwards compatibility (pre CT v1.8)
	     case lists:keysearch(timeout,1,Opts) of
		 {value,{_,T}} -> T;
		 false -> ?DEFAULT_TIMEOUT
	     end
     end,
     case lists:keysearch(total_timeout,1,Opts) of
	 {value,{_,T}} -> T;
	 false -> infinity
     end}.

get_repeat(Opts) ->
    case lists:keysearch(repeat,1,Opts) of
	{value,{repeat,N}} when is_integer(N) ->
	    N;
	false ->
	    case lists:member(repeat,Opts) of
		true ->
		    -1;
		false ->
		    false
	    end
    end.
get_seq(Opts) ->
    lists:member(sequence,Opts).
get_haltpatterns(Opts) ->
    case lists:keysearch(halt,1,Opts) of
	{value,{halt,HaltPatterns}} ->
	    HaltPatterns;
	false ->
	    []
    end.
get_ignore_prompt(Opts) ->    
    lists:member(ignore_prompt,Opts).
get_prompt_check(Opts) ->
    not lists:member(no_prompt_check,Opts).

wait_for_prompt(Pattern, Opts) ->
    case lists:member(wait_for_prompt, Opts) of
	true ->
	    wait_for_prompt1(prompt, Pattern,
			     lists:delete(wait_for_prompt,Opts));
	false ->
	    case proplists:get_value(wait_for_prompt, Opts) of
		undefined ->
		    {false,Pattern,Opts};
		PromptStr ->
		    wait_for_prompt1({prompt,PromptStr}, Pattern,
				     proplists:delete(wait_for_prompt,Opts))
	    end
    end.

wait_for_prompt1(Prompt, [Ch|_] = Pattern, Opts) when is_integer(Ch) ->
    wait_for_prompt2(Prompt, [Pattern], Opts);
wait_for_prompt1(Prompt, Pattern, Opts) when is_list(Pattern) ->
    wait_for_prompt2(Prompt, Pattern, Opts);
wait_for_prompt1(Prompt, Pattern, Opts) ->
    wait_for_prompt2(Prompt, [Pattern], Opts).

wait_for_prompt2(Prompt, Pattern, Opts) ->
    Pattern1 = case lists:reverse(Pattern) of
		   [prompt|_]     -> Pattern;
		   [{prompt,_}|_] -> Pattern;
		   _              -> Pattern ++ [Prompt]
	       end,
    Opts1 = case lists:member(sequence, Opts) of
		true ->  Opts;
		false -> [sequence|Opts]
	    end,
    {true,Pattern1,Opts1}.

%% Repeat either single or sequence. All match results are accumulated
%% and returned when a halt condition is fulfilled.
repeat_expect(_Name,_Pid,Rest,_Pattern,Acc,#eo{repeat=0}) ->
    {ok,lists:reverse(Acc),done,Rest};
repeat_expect(Name,Pid,Data,Pattern,Acc,EO) ->
    case teln_expect1(Name,Pid,Data,Pattern,[],EO) of
	{ok,Matched,Rest} ->
	    EO1 = EO#eo{repeat=EO#eo.repeat-1},
	    repeat_expect(Name,Pid,Rest,Pattern,[Matched|Acc],EO1);
	{halt,Why,Rest} ->
	    {ok,lists:reverse(Acc),Why,Rest};
	{error,Reason} ->
	    {error,Reason}
    end.

teln_expect1(Name,Pid,Data,Pattern,Acc,EO=#eo{idle_timeout=IdleTO,
					      total_timeout=TotalTO}) ->
    %% TotalTO is a float value in this loop (unless it's 'infinity'),
    %% but an integer value will be passed to the other functions
    EOMod = if TotalTO /= infinity -> EO#eo{total_timeout=trunc(TotalTO)};
	       true                -> EO
	    end,
    ExpectFun = case EOMod#eo.seq of
		    true -> fun() ->
				    seq_expect(Name,Pid,Data,Pattern,Acc,EOMod)
			    end;
		    false -> fun() ->
				     one_expect(Name,Pid,Data,Pattern,EOMod)
			     end
		end,
    case ExpectFun() of
	{match,Match,Rest} ->
	    {ok,Match,Rest};
	{halt,Why,Rest} ->
	    {halt,Why,Rest};
	NotFinished ->
	    %% Get more data
	    Fun = fun() -> get_data1(EOMod#eo.teln_pid) end,
	    BreakAfter = if TotalTO < IdleTO ->
				 %% use the integer value
				 EOMod#eo.total_timeout;
			    true ->
				 IdleTO
			 end,
	    {PatOrPats1,Acc1,Rest1} = case NotFinished of
					 {nomatch,Rest0} ->
					     %% one expect
					     {Pattern,[],Rest0};
					 {continue,Pats0,Acc0,Rest0} ->
					     %% sequence
					     {Pats0,Acc0,Rest0}
				     end,
	    case timer:tc(ct_gen_conn, do_within_time, [Fun,BreakAfter]) of
		{_,{error,Reason}} ->
		    %% A timeout will occur when the telnet connection
		    %% is idle for EO#eo.idle_timeout milliseconds.
		    if Rest1 /= [] ->
			    log(name_or_pid(Name,Pid),"       ~ts",[Rest1]);
		       true ->
			    ok
		    end,
		    {error,Reason};
		{_,{ok,Data1}} when TotalTO == infinity ->
		    teln_expect1(Name,Pid,Rest1++Data1,PatOrPats1,Acc1,EOMod);
		{Elapsed,{ok,Data1}} ->
		    TVal = TotalTO - (Elapsed/1000),
		    if TVal =< 0 ->
			    {error,timeout};
		       true ->
			    EO1 = EO#eo{total_timeout = TVal},
			    teln_expect1(Name,Pid,Rest1++Data1,
					 PatOrPats1,Acc1,EO1)
		    end
	    end
    end.

get_data1(Pid) ->
    case ct_telnet_client:get_data(Pid) of
	{ok,[]} ->
	    get_data1(Pid);
	{ok,Data} ->
	    {ok,Data}
    end.

%% 1) Single expect.
%% First the whole data chunk is searched for a prompt (to avoid doing
%% a regexp match for the prompt at each line).
%% If we are searching for anything else, the datachunk is split into
%% lines and each line is matched against each pattern.

%% one_expect: split data chunk at prompts
one_expect(Name,Pid,Data,Pattern,EO) when EO#eo.prompt_check==false ->
%    io:format("Raw Data ~tp Pattern ~tp EO ~tp ",[Data,Pattern,EO]),
    one_expect1(Name,Pid,Data,Pattern,[],EO#eo{found_prompt=false});
one_expect(Name,Pid,Data,Pattern,EO) ->
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    case Pattern of 
		[Prompt] when Prompt==prompt; Prompt=={prompt,PromptType} ->
		    %% Only searching for prompt
		    log_lines(Name,Pid,UptoPrompt),
		    log(name_or_pid(Name,Pid),"PROMPT: ~ts",[PromptType]),
		    {match,{prompt,PromptType},Rest};
		[{prompt,_OtherPromptType}] ->
		    %% Only searching for one specific prompt, not this one
		    log_lines(Name,Pid,UptoPrompt),
		    {nomatch,Rest};
		_ ->
		    one_expect1(Name,Pid,UptoPrompt,Pattern,Rest,
				EO#eo{found_prompt=PromptType})
	    end;
	noprompt ->
	    case Pattern of
		[Prompt] when Prompt==prompt; element(1,Prompt)==prompt ->
		    %% Only searching for prompt
		    LastLine = log_lines_not_last(Name,Pid,Data),
		    {nomatch,LastLine};
		_ ->
		    one_expect1(Name,Pid,Data,Pattern,[],
				EO#eo{found_prompt=false})
	    end
    end.

%% one_expect1: split data chunk at lines
one_expect1(Name,Pid,Data,Pattern,Rest,EO) ->
    case match_lines(Name,Pid,Data,Pattern,EO) of
	{match,Match,MatchRest} ->
	    {match,Match,MatchRest++Rest};
	{nomatch,prompt} ->
	    one_expect(Name,Pid,Rest,Pattern,EO);
	{nomatch,NoMatchRest} ->
	    {nomatch,NoMatchRest++Rest};
	{halt,Why,HaltRest} ->
	    {halt,Why,HaltRest++Rest}
    end.
    

%% 2) Sequence.
%% First the whole data chunk is searched for a prompt (to avoid doing
%% a regexp match for the prompt at each line).
%% If we are searching for anything else, the datachunk is split into
%% lines and each line is matched against the first pattern in the list.
%% When a match is found, the match result is accumulated, and we keep
%% searching for the next pattern in the list.

%% seq_expect: Split data chunk at prompts
seq_expect(_Name,_Pid,Data,[],Acc,_EO) ->
    {match,lists:reverse(Acc),Data};
seq_expect(_Name,_Pid,[],Patterns,Acc,_EO) ->
    {continue,Patterns,lists:reverse(Acc),[]};
seq_expect(Name,Pid,Data,Patterns,Acc,EO) when EO#eo.prompt_check==false ->
    seq_expect1(Name,Pid,Data,Patterns,Acc,[],EO#eo{found_prompt=false});
seq_expect(Name,Pid,Data,Patterns,Acc,EO) ->
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    seq_expect1(Name,Pid,UptoPrompt,Patterns,Acc,Rest,
			EO#eo{found_prompt=PromptType});
	noprompt ->
	    seq_expect1(Name,Pid,Data,Patterns,Acc,[],EO#eo{found_prompt=false})
    end.

%% seq_expect1: For one prompt-chunk, match each pattern - line by
%% line if it is other than the prompt we are searching for.
seq_expect1(Name,Pid,Data,[prompt|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Name,Pid,Data),
	    %% Rest==[] because no prompt is found
	    {continue,[prompt|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Name,Pid,Data),
	    log(name_or_pid(Name,Pid),"PROMPT: ~ts",[PromptType]),
	    seq_expect(Name,Pid,Rest,Patterns,[{prompt,PromptType}|Acc],EO)
    end;
seq_expect1(Name,Pid,Data,[{prompt,PromptType}|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Name,Pid,Data),
	    %% Rest==[] because no prompt is found
	    {continue,[{prompt,PromptType}|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Name,Pid,Data),
	    log(name_or_pid(Name,Pid),"PROMPT: ~ts", [PromptType]),
	    seq_expect(Name,Pid,Rest,Patterns,[{prompt,PromptType}|Acc],EO);
	_OtherPromptType ->
	    log_lines(Name,Pid,Data),
	    seq_expect(Name,Pid,Rest,[{prompt,PromptType}|Patterns],Acc,EO)
    end;
seq_expect1(Name,Pid,Data,[Pattern|Patterns],Acc,Rest,EO) ->
    case match_lines(Name,Pid,Data,[Pattern],EO) of
	{match,Match,MatchRest} ->
	    seq_expect1(Name,Pid,MatchRest,Patterns,[Match|Acc],Rest,EO);
	{nomatch,prompt} ->
	    seq_expect(Name,Pid,Rest,[Pattern|Patterns],Acc,EO);
	{nomatch,NoMatchRest} when Rest==[] ->
	    %% The data did not end with a prompt
	    {continue,[Pattern|Patterns],Acc,NoMatchRest};
	{halt,Why,HaltRest} ->
	    {halt,Why,HaltRest++Rest}
    end;
seq_expect1(_Name,_Pid,Data,[],Acc,Rest,_EO) ->
    {match,lists:reverse(Acc),Data++Rest}.

%% Split prompt-chunk at lines
match_lines(Name,Pid,Data,Patterns,EO) ->
    FoundPrompt = EO#eo.found_prompt,
    case one_line(Data,[]) of
	{noline,Rest} when FoundPrompt=/=false ->
	    %% This is the line including the prompt
	    case match_line(Name,Pid,Rest,Patterns,FoundPrompt,false,EO) of
		nomatch ->
		    {nomatch,prompt};
		{Tag,Match} ->
		    {Tag,Match,[]}
	    end;
	{noline,Rest} when EO#eo.prompt_check==false ->
	    case match_line(Name,Pid,Rest,Patterns,false,false,EO) of
		nomatch ->
		    {nomatch,Rest};
		{Tag,Match} ->
		    {Tag,Match,[]}
	    end;
	{noline,Rest} ->
	    {nomatch,Rest};
	{Line,Rest} ->
	    case match_line(Name,Pid,Line,Patterns,false,true,EO) of
		nomatch ->
		    match_lines(Name,Pid,Rest,Patterns,EO);
		{Tag,Match} ->
		    {Tag,Match,Rest}
	    end
    end.
    
%% For one line, match each pattern
match_line(Name,Pid,Line,Patterns,FoundPrompt,Terminated,EO) ->
    match_line(Name,Pid,Line,Patterns,FoundPrompt,Terminated,EO,match).

match_line(Name,Pid,Line,[prompt|Patterns],false,Term,EO,RetTag) ->
    match_line(Name,Pid,Line,Patterns,false,Term,EO,RetTag);
match_line(Name,Pid,Line,[prompt|_Patterns],FoundPrompt,_Term,_EO,RetTag) ->
    log(name_or_pid(Name,Pid),"       ~ts",[Line]),
    log(name_or_pid(Name,Pid),"PROMPT: ~ts",[FoundPrompt]),
    {RetTag,{prompt,FoundPrompt}};
match_line(Name,Pid,Line,[{prompt,PromptType}|_Patterns],FoundPrompt,_Term,
	   _EO,RetTag) when PromptType==FoundPrompt ->
    log(name_or_pid(Name,Pid),"       ~ts",[Line]),
    log(name_or_pid(Name,Pid),"PROMPT: ~ts",[FoundPrompt]),
    {RetTag,{prompt,FoundPrompt}};
match_line(Name,Pid,Line,[{prompt,PromptType}|Patterns],FoundPrompt,Term,
	   EO,RetTag) 
  when PromptType=/=FoundPrompt ->
    match_line(Name,Pid,Line,Patterns,FoundPrompt,Term,EO,RetTag);
match_line(Name,Pid,Line,[{Tag,Pattern}|Patterns],FoundPrompt,Term,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Name,Pid,Line,Patterns,FoundPrompt,Term,EO,RetTag);
	{match,Match} ->
	    log(name_or_pid(Name,Pid),"MATCH: ~ts",[Line]),
	    {RetTag,{Tag,Match}}
    end;
match_line(Name,Pid,Line,[Pattern|Patterns],FoundPrompt,Term,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Name,Pid,Line,Patterns,FoundPrompt,Term,EO,RetTag);
	{match,Match} ->
	    log(name_or_pid(Name,Pid),"MATCH: ~ts",[Line]),
	    {RetTag,Match}
    end;
match_line(Name,Pid,Line,[],FoundPrompt,Term,EO,match) ->
    match_line(Name,Pid,Line,EO#eo.haltpatterns,FoundPrompt,Term,EO,halt);
%% print any terminated line that cannot be matched
match_line(Name,Pid,Line,[],_FoundPrompt,true,_EO,halt) ->
    log(name_or_pid(Name,Pid),"       ~ts",[Line]),
    nomatch;
%% if there's no line termination, Line is saved as Rest (above) and will
%% be printed later
match_line(_Name,_Pid,_Line,[],_FoundPrompt,false,_EO,halt) ->
    nomatch.

one_line([$\n|Rest],Line) ->
    {lists:reverse(Line),Rest};
one_line([$\r|Rest],Line) ->
    one_line(Rest,Line);
one_line([0|Rest],Line) ->
    one_line(Rest,Line);
one_line([Char|Rest],Line) ->
    one_line(Rest,[Char|Line]);
one_line([],Line) ->
    {noline,lists:reverse(Line)}.

debug_log_lines(String) ->
    Old = put(silent,true),
    log_lines(undefined,undefined,String),
    put(silent,Old).

log_lines(Name,Pid,String) ->
    case log_lines_not_last(Name,Pid,String) of
	[] ->
	    ok;
	LastLine ->
	    log(name_or_pid(Name,Pid),"       ~ts",[LastLine])
    end.

log_lines_not_last(Name,Pid,String) ->
    case add_tabs(String,[],[]) of
	{[],LastLine} ->
	    LastLine;
	{String1,LastLine} ->
	    log(name_or_pid(Name,Pid),"~ts",[String1]),
	    LastLine
    end.

name_or_pid(undefined,Pid) -> Pid;
name_or_pid(Name,_) -> Name.

add_tabs([0|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,LastLine);
add_tabs([$\r|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,LastLine);
add_tabs([$\n|Rest],Acc,LastLine) ->
    add_tabs(Rest,[$\n|LastLine] ++ [$\s,$\s,$\s,$\s,$\s,$\s,$\s|Acc],[]);
add_tabs([Ch|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,[Ch|LastLine]);
add_tabs([],[$\n|Acc],LastLine) ->
    {lists:reverse(Acc),lists:reverse(LastLine)};
add_tabs([],[],LastLine) ->
    {[],lists:reverse(LastLine)}.

-doc false.
teln_receive_until_prompt(Pid,Prx,Timeout) ->
    Fun = fun() -> teln_receive_until_prompt(Pid,Prx,[],[]) end,
    ct_gen_conn:do_within_time(Fun, Timeout).

teln_receive_until_prompt(Pid,Prx,Acc,LastLine) ->
    {ok,Data} = ct_telnet_client:get_data(Pid),
    case check_for_prompt(Prx,LastLine++Data) of
	{prompt,Lines,PromptType,Rest} ->
	    Return = lists:reverse(lists:append([Lines|Acc])),
	   {ok,Return,PromptType,Rest};
	{noprompt,Lines,LastLine1} ->
	    teln_receive_until_prompt(Pid,Prx,[Lines|Acc],LastLine1)
   end.

check_for_prompt(Prx,Data) ->
    case match_prompt(Data,Prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    {RevLines,LastLine} = split_lines(UptoPrompt),
	    {prompt,[LastLine|RevLines],PromptType,Rest};
	noprompt ->
	    {RevLines,Rest} = split_lines(Data),
	    {noprompt,RevLines,Rest}
    end.

split_lines(String) ->
    split_lines(String,[],[]).
split_lines([$\n|Rest],Line,Lines) when Line /= [] ->
    split_lines(Rest,[],[lists:reverse(Line)|Lines]);
split_lines([$\n|Rest],[],Lines) ->
    split_lines(Rest,[],Lines);
split_lines([$\r|Rest],Line,Lines) ->
    split_lines(Rest,Line,Lines);
split_lines([0|Rest],Line,Lines) ->
    split_lines(Rest,Line,Lines);
split_lines([Char|Rest],Line,Lines) ->
    split_lines(Rest,[Char|Line],Lines);
split_lines([],Line,Lines) ->
    {Lines,lists:reverse(Line)}.

match_prompt(Str, Prx) ->
    match_prompt(unicode:characters_to_binary(Str), Prx, []).
match_prompt(Str, Prx, Acc) ->
    case re:run(Str,Prx,[unicode]) of
        nomatch ->
            noprompt;
        {match,[{Start,Len}]} ->
            <<UptoPrompt:Start/binary, Prompt:Len/binary, Rest/binary>> = Str,
            case validate_prompt(Start, UptoPrompt, Prompt) of
                ok ->
                    {prompt,
                     unicode:characters_to_list([lists:reverse(Acc), UptoPrompt, Prompt]),
                     unicode:characters_to_list(Prompt),
                     unicode:characters_to_list(Rest)};
                recurse ->
                    <<Skip:(Start+Len)/binary, Cont/binary>> = Str,
                    match_prompt(Cont, Prx, [Skip|Acc])
            end
    end.

validate_prompt(Size, PrePrompt,  Prompt) ->
    case PrePrompt of
        %% This is a line from "listenv", it is not a real prompt
        <<_:(Size-8)/binary, "PROMPT=\"", _/binary>> ->
            recurse;
        %% This is probably the "Last login:" statement which is
        %% written when telnet connection is opened.
        <<_:(Size-5)/binary, _L:8, "ast ", _/binary>>
          when Prompt =:= <<"login: ">> ->
            recurse;
        _ ->
            ok
    end.
