%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2017. All Rights Reserved.
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

%% @doc Common Test specific layer on top of telnet client `ct_telnet_client.erl'
%%
%% <p>Use this module to set up telnet connections, send commands and
%% perform string matching on the result.
%% See the `unix_telnet' manual page for information about how to use 
%% `ct_telnet', and configure connections, specifically for unix hosts.</p>
%% <p>The following default values are defined in `ct_telnet':</p>
%% <pre>
%% Connection timeout = 10 sec (time to wait for connection)
%% Command timeout = 10 sec (time to wait for a command to return)
%% Max no of reconnection attempts = 3
%% Reconnection interval = 5 sek (time to wait in between reconnection attempts)
%% Keep alive = true (will send NOP to the server every 8 sec if connection is idle)
%% Polling limit = 0 (max number of times to poll to get a remaining string terminated)
%% Polling interval = 1 sec (sleep time between polls)</pre>
%% <p>These parameters can be altered by the user with the following
%% configuration term:</p>
%% <pre>
%% {telnet_settings, [{connect_timeout,Millisec},
%%                    {command_timeout,Millisec},
%%                    {reconnection_attempts,N},
%%                    {reconnection_interval,Millisec},
%%                    {keep_alive,Bool},
%%                    {poll_limit,N},
%%                    {poll_interval,Millisec},
%%                    {tcp_nodelay,Bool}]}.</pre>
%% <p><code>Millisec = integer(), N = integer()</code></p>
%% <p>Enter the <code>telnet_settings</code> term in a configuration 
%% file included in the test and ct_telnet will retrieve the information
%% automatically. Note that `keep_alive' may be specified per connection if
%% required. See `unix_telnet' for details.</p>
%%
%% == Logging ==
%%
%% The default logging behaviour of `ct_telnet' is to print information
%% to the test case HTML log about performed operations and commands
%% and their corresponding results. What won't be printed to the HTML log
%% are text strings sent from the telnet server that are not explicitly
%% received by means of a `ct_telnet' function such as `expect/3'.
%% `ct_telnet' may however be configured to use a special purpose event handler,
%% implemented in `ct_conn_log_h', for logging <b>all</b> telnet traffic.
%% To use this handler, you need to install a Common Test hook named
%% `cth_conn_log'. Example (using the test suite info function):
%%
%% ```
%% suite() ->
%%     [{ct_hooks, [{cth_conn_log, [{conn_mod(),hook_options()}]}]}].
%% '''
%%
%% `conn_mod()' is the name of the common_test module implementing
%% the connection protocol, i.e. `ct_telnet'.
%%
%% The `cth_conn_log' hook performs unformatted logging of telnet data to
%% a separate text file. All telnet communication is captured and printed,
%% including arbitrary data sent from the server. The link to this text file
%% can be found on the top of the test case HTML log.
%%
%% By default, data for all telnet connections is logged in one common
%% file (named `default'), which might get messy e.g. if multiple telnet
%% sessions are running in parallel. It is therefore possible to create a
%% separate log file for each connection. To configure this, use the hook
%% option `hosts' and list the names of the servers/connections that will be
%% used in the suite. Note that the connections must be named for this to work
%% (see the `open' function below).
%%
%% The hook option named `log_type' may be used to change the `cth_conn_log'
%% behaviour. The default value of this option is `raw', which results in the
%% behaviour described above. If the value is set to `html', all telnet
%% communication is printed to the test case HTML log instead.
%%
%% All `cth_conn_log' hook options described above can also be specified in
%% a configuration file with the configuration variable `ct_conn_log'. Example:
%%
%% ```
%% {ct_conn_log, [{ct_telnet,[{log_type,raw},
%%                            {hosts,[key_or_name()]}]}]}
%% '''
%%
%% <b>Note</b> that hook options specified in a configuration file
%% will overwrite any hardcoded hook options in the test suite!
%%
%% === Logging example ===
%%
%% The following `ct_hooks' statement will cause printing of telnet traffic
%% to separate logs for the connections named `server1' and `server2'.
%% Traffic for any other connections will be logged in the default telnet log.
%%
%% ```
%% suite() ->
%%     [{ct_hooks,
%%       [{cth_conn_log, [{ct_telnet,[{hosts,[server1,server2]}]}]}]}].
%%'''
%%
%% As previously explained, the above specification could also be provided
%% by means of an entry like this in a configuration file:
%%
%% ```
%% {ct_conn_log, [{ct_telnet,[{hosts,[server1,server2]}]}]}.
%% '''
%%
%% in which case the `ct_hooks' statement in the test suite may simply look
%% like this:
%%
%% ```
%% suite() ->
%%     [{ct_hooks, [{cth_conn_log, []}]}].
%% '''
%%
%% @end

%% @type connection_type() = telnet | ts1 | ts2

%% @type connection() = handle() |
%% {ct:target_name(),connection_type()} | ct:target_name()

%% @type handle() = ct_gen_conn:handle(). Handle for a
%% specific telnet connection.

%% @type prompt_regexp() = string(). A regular expression which
%% matches all possible prompts for a specific type of target. The
%% regexp must not have any groups i.e. when matching, re:run/3 shall
%% return a list with one single element.
%%
%% @see unix_telnet

-module(ct_telnet).

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

%%%-----------------------------------------------------------------
%%% @spec open(Name) -> {ok,Handle} | {error,Reason}
%%% @equiv open(Name,telnet)
open(Name) ->
    open(Name,telnet).

%%%-----------------------------------------------------------------
%%% @spec open(Name,ConnType) -> {ok,Handle} | {error,Reason}
%%%      Name = target_name()
%%%      ConnType = ct_telnet:connection_type()
%%%      Handle = ct_telnet:handle()
%%%      Reason = term()
%%%
%%% @doc Open a telnet connection to the specified target host.
open(Name,ConnType) ->
    case ct_util:get_key_from_name(Name) of
	{ok, unix} -> % unix host
	    open(Name, ConnType, unix_telnet, Name);
	{ok, Key} -> % any other, e.g. interwatch (iw), etc.
	    open(Name, ConnType, Key, Name);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec open(KeyOrName,ConnType,TargetMod) -> 
%%%                                     {ok,Handle} | {error,Reason}
%%% @equiv open(KeyOrName,ConnType,TargetMod,[])
open(KeyOrName,ConnType,TargetMod) ->
    open(KeyOrName,ConnType,TargetMod,KeyOrName).

%%%-----------------------------------------------------------------
%%% @spec open(KeyOrName,ConnType,TargetMod,Extra) -> 
%%%                                     {ok,Handle} | {error,Reason}
%%%      KeyOrName = Key | Name
%%%      Key = atom()
%%%      Name = ct:target_name()
%%%      ConnType = connection_type()
%%%      TargetMod = atom()
%%%      Extra = term()
%%%      Handle = handle()
%%%      Reason = term()
%%%
%%% @doc Open a telnet connection to the specified target host.
%%%
%%% <p>The target data must exist in a configuration file. The connection 
%%% may be associated with either <code>Name</code> and/or the returned 
%%% <code>Handle</code>. To allocate a name for the target,
%%% use <code>ct:require/2</code> in a test case, or use a 
%%% <code>require</code> statement in the suite info function 
%%% (<code>suite/0</code>), or in a test case info function. 
%%% If you want the connection to be associated with <code>Handle</code> only 
%%% (in case you need to open multiple connections to a host for example), 
%%% simply use <code>Key</code>, the configuration variable name, to 
%%% specify the target. Note that a connection that has no associated target 
%%% name can only be closed with the handle value.</p>
%%% 
%%% <p><code>TargetMod</code> is a module which exports the functions
%%% <code>connect(Ip,Port,KeepAlive,Extra)</code> and <code>get_prompt_regexp()</code>
%%% for the given <code>TargetType</code> (e.g. <code>unix_telnet</code>).</p>
%%%
%%% @see ct:require/2
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

%%%-----------------------------------------------------------------
%%% @spec close(Connection) -> ok | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Reason = term()
%%%
%%% @doc Close the telnet connection and stop the process managing it.
%%% 
%%% <p>A connection may be associated with a target name and/or a handle.
%%% If <code>Connection</code> has no associated target name, it may only
%%% be closed with the handle value (see the <code>open/4</code> 
%%% function).</p>
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
%%% @spec cmd(Connection,Cmd) -> {ok,Data} | {error,Reason}
%%% @equiv cmd(Connection,Cmd,[])
cmd(Connection,Cmd) ->
    cmd(Connection,Cmd,[]).
%%%-----------------------------------------------------------------
%%% @spec cmd(Connection,Cmd,Opts) -> {ok,Data} | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Cmd = string()
%%%      Opts = [Opt]
%%%      Opt = {timeout,timeout()} | {newline,boolean()}
%%%      Data = [string()]
%%%      Reason = term()
%%% @doc Send a command via telnet and wait for prompt.
%%%
%%% <p>This function will by default add a newline to the end of the
%%% given command. If this is not desired, the option
%%% `{newline,false}' can be used. This is necessary, for example,
%%% when sending telnet command sequences (prefixed with the
%%% Interprete As Command, IAC, character).</p>
%%%
%%% <p>The option `timeout' specifies how long the client shall wait for
%%% prompt. If the time expires, the function returns
%%% `{error,timeout}'. See the module description for information
%%% about the default value for the command timeout.</p>
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

%%%-----------------------------------------------------------------
%%% @spec cmdf(Connection,CmdFormat,Args) -> {ok,Data} | {error,Reason}
%%% @equiv cmdf(Connection,CmdFormat,Args,[])
cmdf(Connection,CmdFormat,Args) ->
    cmdf(Connection,CmdFormat,Args,[]).
%%%-----------------------------------------------------------------
%%% @spec cmdf(Connection,CmdFormat,Args,Opts) -> {ok,Data} | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      CmdFormat = string()
%%%      Args = list()
%%%      Opts = [Opt]
%%%      Opt = {timeout,timeout()} | {newline,boolean()}
%%%      Data = [string()]
%%%      Reason = term()
%%% @doc Send a telnet command and wait for prompt 
%%%      (uses a format string and list of arguments to build the command).
%%%
%%% <p>See {@link cmd/3} further description.</p>
cmdf(Connection,CmdFormat,Args,Opts) when is_list(Args) ->
    Cmd = lists:flatten(io_lib:format(CmdFormat,Args)),
    cmd(Connection,Cmd,Opts).

%%%-----------------------------------------------------------------
%%% @spec get_data(Connection) -> {ok,Data} | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Data = [string()]
%%%      Reason = term()
%%% @doc Get all data that has been received by the telnet client
%%% since the last command was sent. Note that only newline terminated
%%% strings are returned. If the last string received has not yet
%%% been terminated, the connection may be polled automatically until
%%% the string is complete. The polling feature is controlled
%%% by the `poll_limit' and `poll_interval' config values and is
%%% by default disabled (meaning the function will immediately
%%% return all complete strings received and save a remaining
%%% non-terminated string for a later `get_data' call).
get_data(Connection) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,get_data);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec send(Connection,Cmd) -> ok | {error,Reason}
%%% @equiv send(Connection,Cmd,[])
send(Connection,Cmd) ->
    send(Connection,Cmd,[]).

%%%-----------------------------------------------------------------
%%% @spec send(Connection,Cmd,Opts) -> ok | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Cmd = string()
%%%      Opts = [Opt]
%%%      Opt = {newline,boolean()}
%%%      Reason = term()
%%% @doc Send a telnet command and return immediately.
%%%
%%% This function will by default add a newline to the end of the
%%% given command. If this is not desired, the option
%%% `{newline,false}' can be used. This is necessary, for example,
%%% when sending telnet command sequences (prefixed with the
%%% Interprete As Command, IAC, character).
%%%
%%% <p>The resulting output from the command can be read with
%%% <code>get_data/1</code> or <code>expect/2/3</code>.</p>
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
check_send_opts([Invalid|_]) ->
    {error,{invalid_option,Invalid}};
check_send_opts([]) ->
    ok.


%%%-----------------------------------------------------------------
%%% @spec sendf(Connection,CmdFormat,Args) -> ok | {error,Reason}
%%% @equiv sendf(Connection,CmdFormat,Args,[])
sendf(Connection,CmdFormat,Args) when is_list(Args) ->
    sendf(Connection,CmdFormat,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec sendf(Connection,CmdFormat,Args,Opts) -> ok | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      CmdFormat = string()
%%%      Args = list()
%%%      Opts = [Opt]
%%%      Opt = {newline,boolean()}
%%%      Reason = term()
%%% @doc Send a telnet command and return immediately (uses a format
%%% string and a list of arguments to build the command).
sendf(Connection,CmdFormat,Args,Opts) when is_list(Args) ->
    Cmd = lists:flatten(io_lib:format(CmdFormat,Args)),
    send(Connection,Cmd,Opts).

%%%-----------------------------------------------------------------
%%% @spec expect(Connection,Patterns) -> term()
%%% @equiv expect(Connections,Patterns,[])
expect(Connection,Patterns) ->
    expect(Connection,Patterns,[]).

%%%-----------------------------------------------------------------
%%% @spec expect(Connection,Patterns,Opts) -> {ok,Match} | 
%%%                                           {ok,MatchList,HaltReason} | 
%%%                                           {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Patterns = Pattern | [Pattern]
%%%      Pattern = string() | {Tag,string()} | prompt | {prompt,Prompt}
%%%      Prompt = string()
%%%      Tag = term()
%%%      Opts = [Opt]
%%%      Opt = {idle_timeout,IdleTimeout} | {total_timeout,TotalTimeout} |
%%%            repeat | {repeat,N} | sequence | {halt,HaltPatterns} |
%%%            ignore_prompt | no_prompt_check | wait_for_prompt |
%%%            {wait_for_prompt,Prompt}
%%%      IdleTimeout = infinity | integer()
%%%      TotalTimeout = infinity | integer()
%%%      N = integer()
%%%      HaltPatterns = Patterns
%%%      MatchList = [Match]
%%%      Match = RxMatch | {Tag,RxMatch} | {prompt,Prompt}
%%%      RxMatch = [string()]
%%%      HaltReason = done | Match
%%%      Reason = timeout | {prompt,Prompt}
%%%
%%% @doc Get data from telnet and wait for the expected pattern.
%%%
%%% <p><code>Pattern</code> can be a POSIX regular expression. The function
%%% returns as soon as a pattern has been successfully matched (at least one,
%%% in the case of multiple patterns).</p>
%%%
%%% <p><code>RxMatch</code> is a list of matched strings. It looks
%%% like this: <code>[FullMatch, SubMatch1, SubMatch2, ...]</code>
%%% where <code>FullMatch</code> is the string matched by the whole
%%% regular expression and <code>SubMatchN</code> is the string that
%%% matched subexpression no <code>N</code>. Subexpressions are
%%% denoted with '(' ')' in the regular expression</p>
%%%
%%% <p>If a <code>Tag</code> is given, the returned <code>Match</code>
%%% will also include the matched <code>Tag</code>. Else, only
%%% <code>RxMatch</code> is returned.</p>
%%%
%%% <p>The <code>idle_timeout</code> option indicates that the function
%%% shall return if the telnet client is idle (i.e. if no data is
%%% received) for more than <code>IdleTimeout</code> milliseconds. Default
%%% timeout is 10 seconds.</p>
%%%
%%% <p>The <code>total_timeout</code> option sets a time limit for
%%% the complete expect operation. After <code>TotalTimeout</code>
%%% milliseconds, <code>{error,timeout}</code> is returned. The default
%%% value is <code>infinity</code> (i.e. no time limit).</p>
%%%
%%% <p>The function will return when a prompt is received, even if no
%%% pattern has yet been matched. In this event,
%%% <code>{error,{prompt,Prompt}}</code> is returned.
%%% However, this behaviour may be modified with the
%%% <code>ignore_prompt</code> or <code>no_prompt_check</code> option, which
%%% tells <code>expect</code> to return only when a match is found or after a
%%% timeout.</p>
%%%
%%% <p>If the <code>ignore_prompt</code> option is used,
%%% <code>ct_telnet</code> will ignore any prompt found. This option
%%% is useful if data sent by the server could include a pattern that
%%% would match the prompt regexp (as returned by
%%% <code>TargedMod:get_prompt_regexp/0</code>), but which should not
%%% cause the function to return.</p>
%%%
%%% <p>If the <code>no_prompt_check</code> option is used,
%%% <code>ct_telnet</code> will not search for a prompt at all. This
%%% is useful if, for instance, the <code>Pattern</code> itself
%%% matches the prompt.</p>
%%%
%%% <p>The <code>wait_for_prompt</code> option forces <code>ct_telnet</code>
%%% to wait until the prompt string has been received before returning
%%% (even if a pattern has already been matched). This is equal to calling:
%%% <code>expect(Conn, Patterns++[{prompt,Prompt}], [sequence|Opts])</code>.
%%% Note that <code>idle_timeout</code> and <code>total_timeout</code>
%%% may abort the operation of waiting for prompt.</p>
%%%
%%% <p>The <code>repeat</code> option indicates that the pattern(s)
%%% shall be matched multiple times. If <code>N</code> is given, the
%%% pattern(s) will be matched <code>N</code> times, and the function
%%% will return with <code>HaltReason = done</code>.</p>
%%%
%%% <p>The <code>sequence</code> option indicates that all patterns
%%% shall be matched in a sequence. A match will not be concluded
%%% untill all patterns are matched.</p>
%%%
%%% <p>Both <code>repeat</code> and <code>sequence</code> can be
%%% interrupted by one or more <code>HaltPatterns</code>. When
%%% <code>sequence</code> or <code>repeat</code> is used, there will
%%% always be a <code>MatchList</code> returned, i.e. a list of
%%% <code>Match</code> instead of only one <code>Match</code>. There
%%% will also be a <code>HaltReason</code> returned.</p>
%%%
%%% <p><underline>Examples:</underline><br/>
%%% <code>expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],</code>
%%% <code>[sequence,{halt,[{nnn,"NNN"}]}]).</code><br/> will try to match
%%% "ABC" first and then "XYZ", but if "NNN" appears the function will
%%% return <code>{error,{nnn,["NNN"]}}</code>. If both "ABC" and "XYZ"
%%% are matched, the function will return
%%% <code>{ok,[AbcMatch,XyzMatch]}</code>.</p>
%%%
%%% <p><code>expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],</code>
%%% <code>[{repeat,2},{halt,[{nnn,"NNN"}]}]).</code><br/> will try to match
%%% "ABC" or "XYZ" twice. If "NNN" appears the function will return
%%% with <code>HaltReason = {nnn,["NNN"]}</code>.</p>
%%%
%%% <p>The <code>repeat</code> and <code>sequence</code> options can be
%%% combined in order to match a sequence multiple times.</p>
expect(Connection,Patterns,Opts) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{expect,Patterns,Opts});
	Error ->
	    Error
    end.

%%%=================================================================
%%% Callback functions
%% @hidden
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

%% @hidden
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


%% @hidden
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


%% @hidden
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
%%% @hidden
log(State,Action,String,Args) when is_record(State, state) ->
    log(State,Action,String,Args,false);
log(Name,Action,String,Args) when is_atom(Name) ->
    log(#state{name=Name},Action,String,Args,false);
log(TelnPid,Action,String,Args) when is_pid(TelnPid) ->
    log(#state{teln_pid=TelnPid},Action,String,Args,false).

%%%-----------------------------------------------------------------
%%% @hidden
log(undefined,String,Args) ->
    log(#state{},undefined,String,Args,false);
log(Name,String,Args) when is_atom(Name) ->
    log(#state{name=Name},undefined,String,Args,false);
log(TelnPid,String,Args) when is_pid(TelnPid) ->
    log(#state{teln_pid=TelnPid},undefined,String,Args).

%%%-----------------------------------------------------------------
%%% @hidden
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
%%% @hidden
start_gen_log(Heading) ->
    %% check if output is suppressed
    case ct_util:is_silenced(telnet) of
	true  -> ok;
	false -> ct_gen_conn:start_log(Heading)
    end.

%%%-----------------------------------------------------------------
%%% @hidden
end_gen_log() -> 
    %% check if output is suppressed
    case ct_util:is_silenced(telnet) of
	true  -> ok;
	false -> ct_gen_conn:end_log()
    end.

%%% @hidden
%% Debug printouts.
debug_cont_gen_log(Str,Args) ->
    Old = put(silent,true),
    ct_gen_conn:cont_log(Str,Args),
    put(silent,Old).

%% Log callback - called from the error handler process
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

%% @hidden
%% @doc Externally the silent_teln_expect function shall only be used
%% by the TargetModule, i.e. the target specific module which
%% implements connect/2 and get_prompt_regexp/0.
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
%% condition is fullfilled.
%% 3b) Repeat (sequence): 2) is repeated either N times or until a
%% halt condition is fullfilled.
teln_expect(Name,Pid,Data,Pattern0,Prx,Opts) ->
    HaltPatterns =
	case get_ignore_prompt(Opts) of
	    true ->
		get_haltpatterns(Opts);
	    false ->
		[prompt | get_haltpatterns(Opts)]
	end,

    PromptCheck = get_prompt_check(Opts),

    {WaitForPrompt,Pattern1,Opts1} = wait_for_prompt(Pattern0,Opts),

    Seq = get_seq(Opts1),
    Pattern2 = convert_pattern(Pattern1,Seq),
    {IdleTimeout,TotalTimeout} = get_timeouts(Opts1),

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
    end.

convert_pattern(Pattern,Seq) 
  when is_list(Pattern) and not is_integer(hd(Pattern)) ->
    case Seq of
	true -> Pattern;
	false -> rm_dupl(Pattern,[])
    end;
convert_pattern(Pattern,_Seq) ->
    [Pattern].

rm_dupl([P|Ps],Acc) ->
    case lists:member(P,Acc) of
	true ->
	    rm_dupl(Ps,Acc);
	false ->
	    rm_dupl(Ps,[P|Acc])
    end;
rm_dupl([],Acc) ->
    lists:reverse(Acc).

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
	    convert_pattern(HaltPatterns,false);
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
%% and returned when a halt condition is fulllfilled.
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
%% If we are searching for anyting else, the datachunk is split into
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
%% line if it is other than the prompt we are seaching for.
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
    case re:run(Line,Pattern,[{capture,all,list},unicode]) of
	nomatch ->
	    match_line(Name,Pid,Line,Patterns,FoundPrompt,Term,EO,RetTag);
	{match,Match} ->
	    log(name_or_pid(Name,Pid),"MATCH: ~ts",[Line]),
	    {RetTag,{Tag,Match}}
    end;
match_line(Name,Pid,Line,[Pattern|Patterns],FoundPrompt,Term,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list},unicode]) of
	nomatch ->
	    match_line(Name,Pid,Line,Patterns,FoundPrompt,Term,EO,RetTag);
	{match,Match} ->
	    log(name_or_pid(Name,Pid),"MATCH: ~ts",[Line]),
	    {RetTag,Match}
    end;
match_line(Name,Pid,Line,[],FoundPrompt,Term,EO,match) ->
    match_line(Name,Pid,Line,EO#eo.haltpatterns,FoundPrompt,Term,EO,halt);
%% print any terminated line that can not be matched
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


%%% @hidden
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


match_prompt(Str,Prx) ->
    match_prompt(Str,Prx,[]).
match_prompt(Str,Prx,Acc) ->
    case re:run(Str,Prx,[unicode]) of
	nomatch ->
	    noprompt;
	{match,[{Start,Len}]} ->
	    case split_prompt_string(Str,Start+1,Start+Len,1,[],[]) of
		{noprompt,Done,Rest} ->
		    match_prompt(Rest,Prx,Done);
		{prompt,UptoPrompt,Prompt,Rest} ->
		    {prompt,lists:reverse(UptoPrompt++Acc),
		     lists:reverse(Prompt),Rest}
	    end
    end.

split_prompt_string([Ch|Str],Start,End,N,UptoPrompt,Prompt) when N<Start ->
    split_prompt_string(Str,Start,End,N+1,[Ch|UptoPrompt],Prompt);
split_prompt_string([Ch|Str],Start,End,N,UptoPrompt,Prompt) 
  when N>=Start, N<End->
    split_prompt_string(Str,Start,End,N+1,UptoPrompt,[Ch|Prompt]);
split_prompt_string([Ch|Rest],_Start,End,N,UptoPrompt,Prompt) when N==End ->
    case UptoPrompt of
	[$",$=,$T,$P,$M,$O,$R,$P|_] ->
	    %% This is a line from "listenv", it is not a real prompt
	    {noprompt,[Ch|Prompt]++UptoPrompt,Rest};
	[$\s,$t,$s,$a|_] when Prompt==":nigol" ->
	    %% This is probably the "Last login:" statement which is
	    %% written when telnet connection is openend.
	    {noprompt,[Ch|Prompt]++UptoPrompt,Rest};
	_ ->
	    {prompt,[Ch|Prompt]++UptoPrompt,[Ch|Prompt],Rest}
    end.
