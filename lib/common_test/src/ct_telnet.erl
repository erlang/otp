%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% @doc Common Test specific layer on top of telnet client ct_telnet_client.erl
%%%
%%% <p>Use this module to set up telnet connections, send commands and
%%% perform string matching on the result.
%%% See the <c>unix_telnet</c> manual page for information about how to use 
%%% ct_telnet, and configure connections, specifically for unix hosts.</p>
%%% <p>The following default values are defined in ct_telnet:</p>
%%% <pre>
%%% Connection timeout = 10 sec (time to wait for connection)
%%% Command timeout = 10 sec (time to wait for a command to return)
%%% Max no of reconnection attempts = 3
%%% Reconnection interval = 5 sek (time to wait in between reconnection attempts)
%%% Keep alive = true (will send NOP to the server every 10 sec if connection is idle)</pre>
%%% <p>These parameters can be altered by the user with the following
%%% configuration term:</p>
%%% <pre>
%%% {telnet_settings, [{connect_timeout,Millisec},
%%%                    {command_timeout,Millisec},
%%%                    {reconnection_attempts,N},
%%%                    {reconnection_interval,Millisec},
%%%                    {keep_alive,Bool}]}.</pre>
%%% <p><code>Millisec = integer(), N = integer()</code></p>
%%% <p>Enter the <code>telnet_settings</code> term in a configuration 
%%% file included in the test and ct_telnet will retrieve the information
%%% automatically. Note that <c>keep_alive</c> may be specified per connection if
%%% required. See <c>unix_telnet</c> for details.</p></doc>

%%% @type connection_type() = telnet | ts1 | ts2

%%% @type connection() = handle() |
%%% {ct:target_name(),connection_type()} | ct:target_name()

%%% @type handle() = ct_gen_conn:handle(). Handle for a
%%% specific telnet connection.

%%% @type prompt_regexp() = string(). A regular expression which
%%% matches all possible prompts for a specific type of target. The
%%% regexp must not have any groups i.e. when matching, re:run/3 shall
%%% return a list with one single element.
%%%
%%% @see unix_telnet

-module(ct_telnet).

-compile(export_all).

-export([open/1, open/2, open/3, open/4, close/1]).
-export([cmd/2, cmd/3, cmdf/3, cmdf/4, get_data/1, 
	 send/2, sendf/3, expect/2, expect/3]).

%% Callbacks
-export([init/3,handle_msg/2,reconnect/2,terminate/2]).

%% Tool internals
-export([silent_teln_expect/5, teln_receive_until_prompt/3,
	 start_log/1, log/3, cont_log/2, end_log/0,
	 try_start_log/1, try_log/3, try_cont_log/2, try_end_log/0]).


-define(RECONNS,3).
-define(RECONN_TIMEOUT,5000).
-define(DEFAULT_TIMEOUT,10000).
-define(DEFAULT_PORT,23).

-include("ct_util.hrl").

-record(state,{teln_pid,
	       prx,
	       type,
	       buffer=[],
	       prompt=false,
	       name,
	       target_mod,
	       keep_alive,
	       extra,
	       conn_to=?DEFAULT_TIMEOUT, 
	       com_to=?DEFAULT_TIMEOUT, 
	       reconns=?RECONNS,
	       reconn_int=?RECONN_TIMEOUT}).

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
open(KeyOrName,ConnType,TargetMod,Extra) ->
    case ct:get_config({KeyOrName,ConnType}) of
	undefined ->
	    log(heading(open,{KeyOrName,ConnType}),"Failed: ~p",
		[{not_available,KeyOrName}]),
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
	    log(heading(open,{KeyOrName,ConnType}),"Opening connection to: ~p",[Addr1]),
	    ct_gen_conn:start(KeyOrName,full_addr(Addr1,ConnType),
			      {TargetMod,KeepAlive,Extra},?MODULE)
    end.

%%%-----------------------------------------------------------------
%%% @spec close(Connection) -> ok | {error,Reason}
%%%       Connection = ct_telnet:connection()
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
	    log("ct_telnet:close","Handle: ~p",[Pid]),
	    case ct_gen_conn:stop(Pid) of
		{error,{process_down,Pid,noproc}} ->
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
%%% @equiv cmd(Connection,Cmd,DefaultTimeout)
cmd(Connection,Cmd) ->
    cmd(Connection,Cmd,default).
%%%-----------------------------------------------------------------
%%% @spec cmd(Connection,Cmd,Timeout) -> {ok,Data} | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Cmd = string()
%%%      Timeout = integer()
%%%      Data = [string()]
%%% @doc Send a command via telnet and wait for prompt.
cmd(Connection,Cmd,Timeout) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{cmd,Cmd,Timeout});
	Error ->
	    Error
    end.
%%%-----------------------------------------------------------------
%%% @spec cmdf(Connection,CmdFormat,Args) -> {ok,Data} | {error,Reason}
%%% @equiv cmdf(Connection,CmdFormat,Args,DefaultTimeout)
cmdf(Connection,CmdFormat,Args) ->
    cmdf(Connection,CmdFormat,Args,default).
%%%-----------------------------------------------------------------
%%% @spec cmdf(Connection,CmdFormat,Args,Timeout) -> {ok,Data} | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      CmdFormat = string()
%%%      Args = list()
%%%      Timeout = integer()
%%%      Data = [string()]
%%% @doc Send a telnet command and wait for prompt 
%%%      (uses a format string and list of arguments to build the command).
%%%-----------------------------------------------------------------
cmdf(Connection,CmdFormat,Args,Timeout) when is_list(Args) ->
    Cmd = lists:flatten(io_lib:format(CmdFormat,Args)),
    cmd(Connection,Cmd,Timeout).

%%%-----------------------------------------------------------------
%%% @spec get_data(Connection) -> {ok,Data} | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Data = [string()]
%%% @doc Get all data which has been received by the telnet client
%%% since last command was sent.
get_data(Connection) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,get_data);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec send(Connection,Cmd) -> ok | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      Cmd = string()
%%% @doc Send a telnet command and return immediately.
%%%
%%% <p>The resulting output from the command can be read with
%%% <code>get_data/1</code> or <code>expect/2/3</code>.</p>
send(Connection,Cmd) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{send,Cmd});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec sendf(Connection,CmdFormat,Args) -> ok | {error,Reason}
%%%      Connection = ct_telnet:connection()
%%%      CmdFormat = string()
%%%      Args = list()
%%% @doc Send a telnet command and return immediately (uses a format
%%% string and a list of arguments to build the command).
sendf(Connection,CmdFormat,Args) when is_list(Args) ->
    Cmd = lists:flatten(io_lib:format(CmdFormat,Args)),
    send(Connection,Cmd).

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
%%%      Opt = {timeout,Timeout} | repeat | {repeat,N} | sequence |
%%%            {halt,HaltPatterns} | ignore_prompt
%%%      Timeout = integer()
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
%%% <p><code>Pattern</code> can be a POSIX regular expression. If more
%%% than one pattern is given, the function returns when the first
%%% match is found.</p>
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
%%% <p>The function will always return when a prompt is found, unless
%%% the <code>ignore_prompt</code> options is used.</p>
%%%
%%% <p>The <code>timeout</code> option indicates that the function
%%% shall return if the telnet client is idle (i.e. if no data is
%%% received) for more than <code>Timeout</code> milliseconds. Default
%%% timeout is 10 seconds.</p>
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
%%% <code>expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],
%%% [sequence,{halt,[{nnn,"NNN"}]}]).</code><br/> will try to match
%%% "ABC" first and then "XYZ", but if "NNN" appears the function will
%%% return <code>{error,{nnn,["NNN"]}}</code>. If both "ABC" and "XYZ"
%%% are matched, the function will return
%%% <code>{ok,[AbcMatch,XyzMatch]}</code>.</p>
%%%
%%% <p><code>expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],
%%% [{repeat,2},{halt,[{nnn,"NNN"}]}]).</code><br/> will try to match
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
    case catch TargetMod:connect(Ip,Port,S0#state.conn_to,KeepAlive,Extra) of
	{ok,TelnPid} ->
	    log(heading(init,{Name,Type}), 
		"Opened telnet connection\n"
		"IP: ~p\n"
		"Port: ~p\n"
		"Command timeout: ~p\n"
		"Reconnection attempts: ~p\n"
		"Reconnection interval: ~p\n"
		"Connection timeout: ~p\n"
		"Keep alive: ~w",
		[Ip,Port,S0#state.com_to,S0#state.reconns,
		 S0#state.reconn_int,S0#state.conn_to,KeepAlive]),
	    {ok,TelnPid,S0#state{teln_pid=TelnPid,
				 type=type(Type),
				 name={Name,Type},
				 target_mod=TargetMod,
				 keep_alive=KeepAlive,
				 extra=Extra,
				 prx=TargetMod:get_prompt_regexp()}};
	{'EXIT',Reason} ->
	    {error,Reason};
	Error ->
	    Error
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
set_telnet_defaults([Unknown|Ss],S) ->
    log(heading(set_telnet_defaults,{telnet_settings,Unknown}),
	"Bad element in telnet_settings: ~p",[Unknown]),
    set_telnet_defaults(Ss,S);
set_telnet_defaults([],S) ->
    S.

%% @hidden
handle_msg({cmd,Cmd,Timeout},State) ->
    try_start_log(heading(cmd,State#state.name)),
    try_cont_log("Cmd: ~p", [Cmd]),
    debug_cont_log("Throwing Buffer:",[]),
    debug_log_lines(State#state.buffer),
    case {State#state.type,State#state.prompt} of
	{ts,_} -> 
	    silent_teln_expect(State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{timeout,2000}]);
	{ip,false} -> 
	    silent_teln_expect(State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{timeout,200}]);
	{ip,true} ->
	    ok
    end,
    TO = if Timeout == default -> State#state.com_to;
	    true -> Timeout
	 end,
    {Return,NewBuffer,Prompt} = 
	case teln_cmd(State#state.teln_pid, Cmd, State#state.prx, TO) of
	    {ok,Data,_PromptType,Rest} ->
		try_cont_log("Return: ~p", [{ok,Data}]),
		{{ok,Data},Rest,true};
	    Error ->
		Retry = {retry,{Error,State#state.name,State#state.teln_pid,
				{cmd,Cmd,TO}}},
		try_cont_log("Return: ~p", [Error]),
		{Retry,[],false}
	end,
    try_end_log(),
    {Return,State#state{buffer=NewBuffer,prompt=Prompt}};
handle_msg({send,Cmd},State) ->
    try_log(heading(send,State#state.name),"Cmd: ~p",[Cmd]),
    debug_cont_log("Throwing Buffer:",[]),
    debug_log_lines(State#state.buffer),
    case {State#state.type,State#state.prompt} of
	{ts,_} -> 
	    silent_teln_expect(State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{timeout,2000}]);
	{ip,false} -> 
	    silent_teln_expect(State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{timeout,200}]);
	{ip,true} ->
	    ok
    end,
    ct_telnet_client:send_data(State#state.teln_pid,Cmd),
    {ok,State#state{buffer=[],prompt=false}};
handle_msg(get_data,State) ->
    try_start_log(heading(get_data,State#state.name)),
    {ok,Data,Buffer} = teln_get_all_data(State#state.teln_pid,
					 State#state.prx,
					 State#state.buffer,
					 [],[]),
    try_cont_log("Return: ~p",[{ok,Data}]),
    try_end_log(),
    {{ok,Data},State#state{buffer=Buffer}};
handle_msg({expect,Pattern,Opts},State) ->
    try_start_log(heading(expect,State#state.name)),
    try_cont_log("Expect: ~p\nOpts=~p\n",[Pattern,Opts]),
    {Return,NewBuffer,Prompt} = 
	case teln_expect(State#state.teln_pid,
			 State#state.buffer,
			 Pattern,
			 State#state.prx,
			 Opts) of
	    {ok,Data,Rest} ->
		P = check_if_prompt_was_reached(Data,[]),
		{{ok,Data},Rest,P};
	    {ok,Data,HaltReason,Rest} ->
		force_cont_log("HaltReason: ~p",
			       [HaltReason]),
		P = check_if_prompt_was_reached(Data,HaltReason),
		{{ok,Data,HaltReason},Rest,P};
	    {error,Reason,Rest} ->
		force_cont_log("Expect failed\n~p",[{error,Reason}]),
		P = check_if_prompt_was_reached([],Reason),
		{{error,Reason},Rest,P};
	    {error,Reason} ->
		force_cont_log("Expect failed\n~p",[{error,Reason}]),
		P = check_if_prompt_was_reached([],Reason),
		{{error,Reason},[],P}
	end,
    try_end_log(),
    Return1 = case Return of
		  {error,_} -> {retry,{Return,State#state.name,
				       State#state.teln_pid,
				       {expect,Pattern,Opts}}};
		  _ -> Return
	      end,	    
    {Return1,State#state{buffer=NewBuffer,prompt=Prompt}}.


%% @hidden
reconnect({Ip,Port,_Type},State) ->
    reconnect(Ip,Port,State#state.reconns,State).
reconnect(Ip,Port,N,State=#state{target_mod=TargetMod,
				 keep_alive=KeepAlive,
				 extra=Extra,
				 conn_to=ConnTo,
				 reconn_int=ReconnInt}) ->
    case TargetMod:connect(Ip,Port,ConnTo,KeepAlive,Extra) of
	{ok, NewPid} ->
	    {ok, NewPid, State#state{teln_pid=NewPid}};
	Error when N==0 ->
	    Error;
	_Error ->
	    log("Reconnect failed!","Retries left: ~p",[N]),
	    timer:sleep(ReconnInt),
	    reconnect(Ip,Port,N-1,State)
    end.


%% @hidden
terminate(TelnPid,State) ->
    log(heading(terminate,State#state.name),
	"Closing telnet connection.\nId: ~p",
	[TelnPid]),
    ct_telnet_client:close(TelnPid).


%%%=================================================================
%%% Internal function
get_handle(Pid) when is_pid(Pid) ->
    {ok,Pid};
get_handle({Name,Type}) when Type==telnet;Type==ts1;Type==ts2 ->
    case ct_util:get_connections(Name,?MODULE) of
	{ok,Conns} when Conns /= [] ->
	    case get_handle(Type,Conns) of
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
	{ok,[]} ->
	    {error,already_closed};
	Error ->
	    Error
    end;
get_handle(Name) ->
    get_handle({Name,telnet}).

get_handle(Type,[{Pid,{_,_,Type}}|_]) ->
    {ok,Pid};
get_handle(Type,[_H|T]) ->
    get_handle(Type,T);
get_handle(Type,[]) ->
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

%tc(Fun) ->
%    Before = erlang:now(),
%    Val = Fun(),
%    After = erlang:now(),
%    {now_diff(After, Before), Val}.
%now_diff({A2, B2, C2}, {A1, B1, C1}) ->
%    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

heading(Function,Name) ->
    io_lib:format("~w:~w ~p",[?MODULE,Function,Name]).

%%% @hidden
%% Functions for regular (unconditional) logging, to be
%% used during connect, reconnect, disconnect etc.
log(Heading,Str,Args) ->
    ct_gen_conn:log(Heading,Str,Args).
%%% @hidden
start_log(Heading) -> 
    ct_gen_conn:start_log(Heading).
cont_log(Str,Args) -> 
    ct_gen_conn:cont_log(Str,Args).
end_log() -> 
    ct_gen_conn:end_log().

%%% @hidden
%% Functions for conditional logging, to be used by
%% cmd, send, receive, expect etc (this output may be 
%% silenced by user).
try_start_log(Heading) -> 
    do_try_log(start_log,[Heading]).
%%% @hidden
try_end_log() -> 
    do_try_log(end_log,[]).

%%% @hidden
try_log(Heading,Str,Args) ->
    do_try_log(log,[Heading,Str,Args]).

%%% @hidden
try_cont_log(Str,Args) -> 
    do_try_log(cont_log,[Str,Args]).

%%% @hidden
do_try_log(Func,Args) ->
    %% check if output is suppressed
    case ct_util:is_silenced(telnet) of
	true ->
	    ok;
	false ->
	    apply(ct_gen_conn,Func,Args)
    end.

%%% @hidden
%% Functions that will force printout even if ct_telnet
%% output has been silenced, to be used for error printouts.
force_cont_log(Str,Args) ->
    case ct_util:is_silenced(telnet) of
	true ->
	    %% call log/3 now instead of cont_log/2 since 
	    %% start_log/1 will not have been previously called
	    log("ct_telnet info",Str,Args);
	false ->
	    cont_log(Str,Args)
    end.

%%% @hidden
%% Debug printouts.
debug_cont_log(Str,Args) ->
    Old = put(silent,true),
    cont_log(Str,Args),
    put(silent,Old).



%%%=================================================================
%%% Abstraction layer on top of ct_telnet_client.erl
teln_cmd(Pid,Cmd,Prx,Timeout) ->
    ct_telnet_client:send_data(Pid,Cmd),
    teln_receive_until_prompt(Pid,Prx,Timeout).


teln_get_all_data(Pid,Prx,Data,Acc,LastLine) ->
    case check_for_prompt(Prx,lists:reverse(LastLine) ++ Data) of
	{prompt,Lines,_PromptType,Rest} ->
	    teln_get_all_data(Pid,Prx,Rest,[Lines|Acc],[]);
	{noprompt,Lines,LastLine1} ->
	    case ct_telnet_client:get_data(Pid) of
		{ok,[]} ->
		    {ok,lists:reverse(lists:append([Lines|Acc])),
		     lists:reverse(LastLine1)};
		{ok,Data1} ->
		    teln_get_all_data(Pid,Prx,Data1,[Lines|Acc],LastLine1)
	    end
    end.
    
%% Expect options record
-record(eo,{teln_pid,
	    prx,
	    timeout,
	    haltpatterns=[],
	    seq=false,
	    repeat=false,
	    found_prompt=false}).

%% @hidden
%% @doc Externally the silent_teln_expect function shall only be used
%% by the TargetModule, i.e. the target specific module which
%% implements connect/2 and get_prompt_regexp/0.
silent_teln_expect(Pid,Data,Pattern,Prx,Opts) ->
    Old = put(silent,true),
    try_cont_log("silent_teln_expect/5, Pattern = ~p",[Pattern]),
    Result = teln_expect(Pid,Data,Pattern,Prx,Opts),
    try_cont_log("silent_teln_expect -> ~p\n",[Result]),
    put(silent,Old),
    Result.

%% teln_expect/5 
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
teln_expect(Pid,Data,Pattern0,Prx,Opts) -> HaltPatterns = case
    get_ignore_prompt(Opts) of true -> get_haltpatterns(Opts); false
    -> [prompt | get_haltpatterns(Opts)] end,

    Seq = get_seq(Opts),
    Pattern = convert_pattern(Pattern0,Seq),

    Timeout = get_timeout(Opts),
 
    EO = #eo{teln_pid=Pid,
	     prx=Prx,
	     timeout=Timeout,
	     seq=Seq,
	     haltpatterns=HaltPatterns},
    
    case get_repeat(Opts) of
	false ->
	    case teln_expect1(Data,Pattern,[],EO) of
		{ok,Matched,Rest} ->
		    {ok,Matched,Rest};
		{halt,Why,Rest} ->
		    {error,Why,Rest};
		{error,Reason} ->
		    {error,Reason}
	    end;
	N ->
	    EO1 = EO#eo{repeat=N},
	    repeat_expect(Data,Pattern,[],EO1)
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

get_timeout(Opts) ->
    case lists:keysearch(timeout,1,Opts) of
	{value,{timeout,T}} -> T;
	false -> ?DEFAULT_TIMEOUT
    end.
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
	
%% Repeat either single or sequence. All match results are accumulated
%% and returned when a halt condition is fulllfilled.
repeat_expect(Rest,_Pattern,Acc,#eo{repeat=0}) ->
    {ok,lists:reverse(Acc),done,Rest};
repeat_expect(Data,Pattern,Acc,EO) ->
    case teln_expect1(Data,Pattern,[],EO) of
	{ok,Matched,Rest} ->
	    EO1 = EO#eo{repeat=EO#eo.repeat-1},
	    repeat_expect(Rest,Pattern,[Matched|Acc],EO1);
	{halt,Why,Rest} ->
	    {ok,lists:reverse(Acc),Why,Rest};
	{error,Reason} ->
	    {error,Reason}
    end.

teln_expect1(Data,Pattern,Acc,EO) ->
    ExpectFun = case EO#eo.seq of
		    true -> fun() -> seq_expect(Data,Pattern,Acc,EO) end;
		    false -> fun() -> one_expect(Data,Pattern,EO) end
		end,
    case ExpectFun() of
	{match,Match,Rest} ->
	    {ok,Match,Rest};
	{halt,Why,Rest} ->
	    {halt,Why,Rest};
	NotFinished ->
	    %% Get more data
	    Fun = fun() -> get_data1(EO#eo.teln_pid) end,
	    case ct_gen_conn:do_within_time(Fun, EO#eo.timeout) of
		{error,Reason} -> 
		    %% A timeout will occur when the telnet connection
		    %% is idle for EO#eo.timeout milliseconds.
		    {error,Reason};
		{ok,Data1} ->
		    case NotFinished of
			{nomatch,Rest} ->
			    %% One expect
			    teln_expect1(Rest++Data1,Pattern,[],EO);
			{continue,Patterns1,Acc1,Rest} ->
			    %% Sequence
			    teln_expect1(Rest++Data1,Patterns1,Acc1,EO)
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
%% If we are searching for anyting else, the datachunk is split into
%% lines and each line is matched against each pattern.

%% one_expect: split data chunk at prompts
one_expect(Data,Pattern,EO) ->
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    case Pattern of 
		[Prompt] when Prompt==prompt; Prompt=={prompt,PromptType} ->
		    %% Only searching for prompt
		    log_lines(UptoPrompt),
		    try_cont_log("<b>PROMPT:</b> ~s", [PromptType]),
		    {match,{prompt,PromptType},Rest};
		[{prompt,_OtherPromptType}] ->
		    %% Only searching for one specific prompt, not thisone
		    log_lines(UptoPrompt),
		    {nomatch,Rest};
		_ ->
		    one_expect1(UptoPrompt,Pattern,Rest,
				EO#eo{found_prompt=PromptType})
	    end;
	noprompt ->
	    case Pattern of
		[Prompt] when Prompt==prompt; element(1,Prompt)==prompt ->
		    %% Only searching for prompt
		    LastLine = log_lines_not_last(Data),
		    {nomatch,LastLine};
		_ ->
		    one_expect1(Data,Pattern,[],EO#eo{found_prompt=false})
	    end
    end.

remove_zero(List) ->
    [Ch || Ch <- List, Ch=/=0, Ch=/=13].

%% one_expect1: split data chunk at lines
one_expect1(Data,Pattern,Rest,EO) ->
    case match_lines(Data,Pattern,EO) of
	{match,Match,MatchRest} ->
	    {match,Match,MatchRest++Rest};
	{nomatch,prompt} ->
	    one_expect(Rest,Pattern,EO);
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
seq_expect(Data,[],Acc,_EO) ->
    {match,lists:reverse(Acc),Data};
seq_expect([],Patterns,Acc,_EO) ->
    {continue,Patterns,lists:reverse(Acc),[]};
seq_expect(Data,Patterns,Acc,EO) ->
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    seq_expect1(UptoPrompt,Patterns,Acc,Rest,
			EO#eo{found_prompt=PromptType});
	noprompt ->
	    seq_expect1(Data,Patterns,Acc,[],EO#eo{found_prompt=false})
    end.

%% seq_expect1: For one prompt-chunk, match each pattern - line by
%% line if it is other than the prompt we are seaching for.
seq_expect1(Data,[prompt|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Data),
	    %% Rest==[] because no prompt is found
	    {continue,[prompt|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Data),
	    try_cont_log("<b>PROMPT:</b> ~s", [PromptType]),
	    seq_expect(Rest,Patterns,[{prompt,PromptType}|Acc],EO)
    end;
seq_expect1(Data,[{prompt,PromptType}|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Data),
	    %% Rest==[] because no prompt is found
	    {continue,[{prompt,PromptType}|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Data),
	    try_cont_log("<b>PROMPT:</b> ~s", [PromptType]),
	    seq_expect(Rest,Patterns,[{prompt,PromptType}|Acc],EO);
	_OtherPromptType ->
	    log_lines(Data),
	    seq_expect(Rest,[{prompt,PromptType}|Patterns],Acc,EO)
    end;
seq_expect1(Data,[Pattern|Patterns],Acc,Rest,EO) ->
    case match_lines(Data,[Pattern],EO) of
	{match,Match,MatchRest} ->
	    seq_expect1(MatchRest,Patterns,[Match|Acc],Rest,EO);
	{nomatch,prompt} ->
	    seq_expect(Rest,[Pattern|Patterns],Acc,EO);
	{nomatch,NoMatchRest} when Rest==[] ->
	    %% The data did not end with a prompt
	    {continue,[Pattern|Patterns],Acc,NoMatchRest};
	{halt,Why,HaltRest} ->
	    {halt,Why,HaltRest++Rest}
    end;
seq_expect1(Data,[],Acc,Rest,_EO) ->
    {match,lists:reverse(Acc),Data++Rest}.

%% Split prompt-chunk at lines
match_lines(Data,Patterns,EO) ->
    FoundPrompt = EO#eo.found_prompt,
    case one_line(Data,[]) of
	{noline,Rest} when FoundPrompt=/=false ->
	    %% This is the line including the prompt
	    case match_line(Rest,Patterns,FoundPrompt,EO) of
		nomatch ->
		    {nomatch,prompt};
		{Tag,Match} ->
		    {Tag,Match,[]}
	    end;
	{noline,Rest} ->
	    {nomatch,Rest};
	{Line,Rest} ->
	    case match_line(Line,Patterns,false,EO) of
		nomatch ->
		    match_lines(Rest,Patterns,EO);
		{Tag,Match} ->
		    {Tag,Match,Rest}
	    end
    end.
    

%% For one line, match each pattern
match_line(Line,Patterns,FoundPrompt,EO) ->
    match_line(Line,Patterns,FoundPrompt,EO,match).

match_line(Line,[prompt|Patterns],false,EO,RetTag) ->
    match_line(Line,Patterns,false,EO,RetTag);
match_line(Line,[prompt|_Patterns],FoundPrompt,_EO,RetTag) ->
    try_cont_log("       ~s", [Line]),
    try_cont_log("<b>PROMPT:</b> ~s", [FoundPrompt]),
    {RetTag,{prompt,FoundPrompt}};
match_line(Line,[{prompt,PromptType}|_Patterns],FoundPrompt,_EO,RetTag) 
  when PromptType==FoundPrompt ->
    try_cont_log("       ~s", [Line]),
    try_cont_log("<b>PROMPT:</b> ~s", [FoundPrompt]),
    {RetTag,{prompt,FoundPrompt}};
match_line(Line,[{prompt,PromptType}|Patterns],FoundPrompt,EO,RetTag) 
  when PromptType=/=FoundPrompt ->
    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
match_line(Line,[{Tag,Pattern}|Patterns],FoundPrompt,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
	{match,Match} ->
	    try_cont_log("<b>MATCH:</b> ~s", [Line]),
	    {RetTag,{Tag,Match}}
    end;
match_line(Line,[Pattern|Patterns],FoundPrompt,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
	{match,Match} ->
	    try_cont_log("<b>MATCH:</b> ~s", [Line]),
	    {RetTag,Match}
    end;
match_line(Line,[],FoundPrompt,EO,match) ->
    match_line(Line,EO#eo.haltpatterns,FoundPrompt,EO,halt);
match_line(Line,[],_FoundPrompt,_EO,halt) ->
    try_cont_log("       ~s", [Line]),
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
    log_lines(String),
    put(silent,Old).

log_lines(String) ->
    case log_lines_not_last(String) of
	[] ->
	    ok;
	LastLine ->
	    try_cont_log("       ~s", [LastLine])
    end.

log_lines_not_last(String) ->
    case add_tabs(String,[],[]) of
	{[],LastLine} ->
	    LastLine;
	{String1,LastLine} ->
	    try_cont_log("~s",[String1]),
	    LastLine
    end.

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
    case check_for_prompt(Prx,LastLine ++ Data) of
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
split_lines([$\n|Rest],Line,Lines) ->
    split_lines(Rest,[],[lists:reverse(Line)|Lines]);
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
    case re:run(Str,Prx) of
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
