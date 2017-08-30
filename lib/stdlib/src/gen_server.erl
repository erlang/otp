%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(gen_server).

%%% ---------------------------------------------------
%%%
%%% The idea behind THIS server is that the user module
%%% provides (different) functions to handle different
%%% kind of inputs. 
%%% If the Parent process terminates the Module:terminate/2
%%% function is called.
%%%
%%% The user module should export:
%%%
%%%   init(Args)  
%%%     ==> {ok, State}
%%%         {ok, State, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   handle_call(Msg, {From, Tag}, State)
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}  
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_cast(Msg, State)
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State} 
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, State) Info is e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State} 
%%%              Reason = normal | shutdown | Term, terminate(State) is called
%%%
%%%   terminate(Reason, State) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> ok
%%%
%%%
%%% The work flow (of the server) can be described as follows:
%%%
%%%   User module                          Generic
%%%   -----------                          -------
%%%     start            ----->             start
%%%     init             <-----              .
%%%
%%%                                         loop
%%%     handle_call      <-----              .
%%%                      ----->             reply
%%%
%%%     handle_cast      <-----              .
%%%
%%%     handle_info      <-----              .
%%%
%%%     terminate        <-----              .
%%%
%%%                      ----->             reply
%%%
%%%
%%% ---------------------------------------------------

%% API
-export([start/3, start/4,
	 start_link/3, start_link/4,
	 stop/1, stop/3,
	 call/2, call/3,
	 cast/2, reply/2,
	 abcast/2, abcast/3,
	 multi_call/2, multi_call/3, multi_call/4,
	 enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/5]).

%% System exports
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 system_get_state/1,
	 system_replace_state/2,
	 format_status/2]).

%% Internal exports
-export([init_it/6]).

-import(error_logger, [format/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-optional_callbacks([format_status/2]).


%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, atom()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{timeout, Timeout} | {debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).


%% -----------------------------------------------------------------
%% Stop a generic server and wait for it to terminate.
%% If the server is located at another node, that node will
%% be monitored.
%% -----------------------------------------------------------------
stop(Name) ->
    gen:stop(Name).

stop(Name, Reason, Timeout) ->
    gen:stop(Name, Reason, Timeout).

%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on timeouts) ?).
%% ----------------------------------------------------------------- 
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.

%% -----------------------------------------------------------------
%% Make a cast to a generic server.
%% -----------------------------------------------------------------
cast({global,Name}, Request) ->
    catch global:send(Name, cast_msg(Request)),
    ok;
cast({via, Mod, Name}, Request) ->
    catch Mod:send(Name, cast_msg(Request)),
    ok;
cast({Name,Node}=Dest, Request) when is_atom(Name), is_atom(Node) -> 
    do_cast(Dest, Request);
cast(Dest, Request) when is_atom(Dest) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_pid(Dest) ->
    do_cast(Dest, Request).

do_cast(Dest, Request) -> 
    do_send(Dest, cast_msg(Request)),
    ok.
    
cast_msg(Request) -> {'$gen_cast',Request}.

%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%% ----------------------------------------------------------------- 
%% Asynchronous broadcast, returns nothing, it's just send 'n' pray
%%-----------------------------------------------------------------  
abcast(Name, Request) when is_atom(Name) ->
    do_abcast([node() | nodes()], Name, cast_msg(Request)).

abcast(Nodes, Name, Request) when is_list(Nodes), is_atom(Name) ->
    do_abcast(Nodes, Name, cast_msg(Request)).

do_abcast([Node|Nodes], Name, Msg) when is_atom(Node) ->
    do_send({Name,Node},Msg),
    do_abcast(Nodes, Name, Msg);
do_abcast([], _,_) -> abcast.

%%% -----------------------------------------------------------------
%%% Make a call to servers at several nodes.
%%% Returns: {[Replies],[BadNodes]}
%%% A Timeout can be given
%%% 
%%% A middleman process is used in case late answers arrives after
%%% the timeout. If they would be allowed to glog the callers message
%%% queue, it would probably become confused. Late answers will 
%%% now arrive to the terminated middleman and so be discarded.
%%% -----------------------------------------------------------------
multi_call(Name, Req)
  when is_atom(Name) ->
    do_multi_call([node() | nodes()], Name, Req, infinity).

multi_call(Nodes, Name, Req) 
  when is_list(Nodes), is_atom(Name) ->
    do_multi_call(Nodes, Name, Req, infinity).

multi_call(Nodes, Name, Req, infinity) ->
    do_multi_call(Nodes, Name, Req, infinity);
multi_call(Nodes, Name, Req, Timeout) 
  when is_list(Nodes), is_atom(Name), is_integer(Timeout), Timeout >= 0 ->
    do_multi_call(Nodes, Name, Req, Timeout).


%%-----------------------------------------------------------------
%% enter_loop(Mod, Options, State, <ServerName>, <TimeOut>) ->_ 
%%   
%% Description: Makes an existing process into a gen_server. 
%%              The calling process will enter the gen_server receive 
%%              loop and become a gen_server process.
%%              The process *must* have been started using one of the 
%%              start functions in proc_lib, see proc_lib(3). 
%%              The user is responsible for any initialization of the 
%%              process, including registering a name for it.
%%-----------------------------------------------------------------
enter_loop(Mod, Options, State) ->
    enter_loop(Mod, Options, State, self(), infinity).

enter_loop(Mod, Options, State, ServerName = {Scope, _})
  when Scope == local; Scope == global ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, ServerName = {via, _, _}) ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, Timeout) ->
    enter_loop(Mod, Options, State, self(), Timeout).

enter_loop(Mod, Options, State, ServerName, Timeout) ->
    Name = gen:get_proc_name(ServerName),
    Parent = gen:get_parent(),
    Debug = gen:debug_options(Name, Options),
    loop(Parent, Name, State, Mod, Timeout, Debug).

%%%========================================================================
%%% Gen-callback functions
%%%========================================================================

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = gen:name(Name0),
    Debug = gen:debug_options(Name, Options),
    case catch Mod:init(Args) of
	{ok, State} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, infinity, Debug);
	{ok, State, Timeout} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, Timeout, Debug);
	{stop, Reason} ->
	    %% For consistency, we must make sure that the
	    %% registered name (if any) is unregistered before
	    %% the parent process is notified about the failure.
	    %% (Otherwise, the parent process could get
	    %% an 'already_started' error if it immediately
	    %% tried starting the process again.)
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------
loop(Parent, Name, State, Mod, hibernate, Debug) ->
    proc_lib:hibernate(?MODULE,wake_hib,[Parent, Name, State, Mod, Debug]);
loop(Parent, Name, State, Mod, Time, Debug) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  timeout
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, false).

wake_hib(Parent, Name, State, Mod, Debug) ->
    Msg = receive
	      Input ->
		  Input
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, hibernate, Debug, true).

decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, Hib) ->
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, State, Mod, Time], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Name, Msg, Mod, State, Debug);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, Name, State, Mod);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {in, Msg}),
	    handle_msg(Msg, Parent, Name, State, Mod, Debug1)
    end.

%%% ---------------------------------------------------
%%% Send/receive functions
%%% ---------------------------------------------------
do_send(Dest, Msg) ->
    case catch erlang:send(Dest, Msg, [noconnect]) of
	noconnect ->
	    spawn(erlang, send, [Dest,Msg]);
	Other ->
	    Other
    end.

do_multi_call(Nodes, Name, Req, infinity) ->
    Tag = make_ref(),
    Monitors = send_nodes(Nodes, Name, Tag, Req),
    rec_nodes(Tag, Monitors, Name, undefined);
do_multi_call(Nodes, Name, Req, Timeout) ->
    Tag = make_ref(),
    Caller = self(),
    Receiver =
	spawn(
	  fun() ->
		  %% Middleman process. Should be unsensitive to regular
		  %% exit signals. The sychronization is needed in case
		  %% the receiver would exit before the caller started
		  %% the monitor.
		  process_flag(trap_exit, true),
		  Mref = erlang:monitor(process, Caller),
		  receive
		      {Caller,Tag} ->
			  Monitors = send_nodes(Nodes, Name, Tag, Req),
			  TimerId = erlang:start_timer(Timeout, self(), ok),
			  Result = rec_nodes(Tag, Monitors, Name, TimerId),
			  exit({self(),Tag,Result});
		      {'DOWN',Mref,_,_,_} ->
			  %% Caller died before sending us the go-ahead.
			  %% Give up silently.
			  exit(normal)
		  end
	  end),
    Mref = erlang:monitor(process, Receiver),
    Receiver ! {self(),Tag},
    receive
	{'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
	    Result;
	{'DOWN',Mref,_,_,Reason} ->
	    %% The middleman code failed. Or someone did 
	    %% exit(_, kill) on the middleman process => Reason==killed
	    exit(Reason)
    end.

send_nodes(Nodes, Name, Tag, Req) ->
    send_nodes(Nodes, Name, Tag, Req, []).

send_nodes([Node|Tail], Name, Tag, Req, Monitors)
  when is_atom(Node) ->
    Monitor = start_monitor(Node, Name),
    %% Handle non-existing names in rec_nodes.
    catch {Name, Node} ! {'$gen_call', {self(), {Tag, Node}}, Req},
    send_nodes(Tail, Name, Tag, Req, [Monitor | Monitors]);
send_nodes([_Node|Tail], Name, Tag, Req, Monitors) ->
    %% Skip non-atom Node
    send_nodes(Tail, Name, Tag, Req, Monitors);
send_nodes([], _Name, _Tag, _Req, Monitors) -> 
    Monitors.

%% Against old nodes:
%% If no reply has been delivered within 2 secs. (per node) check that
%% the server really exists and wait for ever for the answer.
%%
%% Against contemporary nodes:
%% Wait for reply, server 'DOWN', or timeout from TimerId.

rec_nodes(Tag, Nodes, Name, TimerId) -> 
    rec_nodes(Tag, Nodes, Name, [], [], 2000, TimerId).

rec_nodes(Tag, [{N,R}|Tail], Name, Badnodes, Replies, Time, TimerId ) ->
    receive
	{'DOWN', R, _, _, _} ->
	    rec_nodes(Tag, Tail, Name, [N|Badnodes], Replies, Time, TimerId);
	{{Tag, N}, Reply} ->  %% Tag is bound !!!
	    erlang:demonitor(R, [flush]),
	    rec_nodes(Tag, Tail, Name, Badnodes, 
		      [{N,Reply}|Replies], Time, TimerId);
	{timeout, TimerId, _} ->	
	    erlang:demonitor(R, [flush]),
	    %% Collect all replies that already have arrived
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
    end;
rec_nodes(Tag, [N|Tail], Name, Badnodes, Replies, Time, TimerId) ->
    %% R6 node
    receive
	{nodedown, N} ->
	    monitor_node(N, false),
	    rec_nodes(Tag, Tail, Name, [N|Badnodes], Replies, 2000, TimerId);
	{{Tag, N}, Reply} ->  %% Tag is bound !!!
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    rec_nodes(Tag, Tail, Name, Badnodes,
		      [{N,Reply}|Replies], 2000, TimerId);
	{timeout, TimerId, _} ->	
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    %% Collect all replies that already have arrived
	    rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies)
    after Time ->
	    case rpc:call(N, erlang, whereis, [Name]) of
		Pid when is_pid(Pid) -> % It exists try again.
		    rec_nodes(Tag, [N|Tail], Name, Badnodes,
			      Replies, infinity, TimerId);
		_ -> % badnode
		    receive {nodedown, N} -> ok after 0 -> ok end,
		    monitor_node(N, false),
		    rec_nodes(Tag, Tail, Name, [N|Badnodes],
			      Replies, 2000, TimerId)
	    end
    end;
rec_nodes(_, [], _, Badnodes, Replies, _, TimerId) ->
    case catch erlang:cancel_timer(TimerId) of
	false ->  % It has already sent it's message
	    receive
		{timeout, TimerId, _} -> ok
	    after 0 ->
		    ok
	    end;
	_ -> % Timer was cancelled, or TimerId was 'undefined'
	    ok
    end,
    {Replies, Badnodes}.

%% Collect all replies that already have arrived
rec_nodes_rest(Tag, [{N,R}|Tail], Name, Badnodes, Replies) ->
    receive
	{'DOWN', R, _, _, _} ->
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies);
	{{Tag, N}, Reply} -> %% Tag is bound !!!
	    erlang:demonitor(R, [flush]),
	    rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N,Reply}|Replies])
    after 0 ->
	    erlang:demonitor(R, [flush]),
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
    end;
rec_nodes_rest(Tag, [N|Tail], Name, Badnodes, Replies) ->
    %% R6 node
    receive
	{nodedown, N} ->
	    monitor_node(N, false),
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies);
	{{Tag, N}, Reply} ->  %% Tag is bound !!!
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N,Reply}|Replies])
    after 0 ->
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
    end;
rec_nodes_rest(_Tag, [], _Name, Badnodes, Replies) ->
    {Replies, Badnodes}.


%%% ---------------------------------------------------
%%% Monitor functions
%%% ---------------------------------------------------

start_monitor(Node, Name) when is_atom(Node), is_atom(Name) ->
    if node() =:= nonode@nohost, Node =/= nonode@nohost ->
	    Ref = make_ref(),
	    self() ! {'DOWN', Ref, process, {Name, Node}, noconnection},
	    {Node, Ref};
       true ->
	    case catch erlang:monitor(process, {Name, Node}) of
		{'EXIT', _} ->
		    %% Remote node is R6
		    monitor_node(Node, true),
		    Node;
		Ref when is_reference(Ref) ->
		    {Node, Ref}
	    end
    end.

%% ---------------------------------------------------
%% Helper functions for try-catch of callbacks.
%% Returns the return value of the callback, or
%% {'EXIT', ExitReason, ReportReason} (if an exception occurs)
%%
%% ExitReason is the reason that shall be used when the process
%% terminates.
%%
%% ReportReason is the reason that shall be printed in the error
%% report.
%%
%% These functions are introduced in order to add the stack trace in
%% the error report produced when a callback is terminated with
%% erlang:exit/1 (OTP-12263).
%% ---------------------------------------------------

try_dispatch({'$gen_cast', Msg}, Mod, State) ->
    try_dispatch(Mod, handle_cast, Msg, State);
try_dispatch(Info, Mod, State) ->
    try_dispatch(Mod, handle_info, Info, State).

try_dispatch(Mod, Func, Msg, State) ->
    try
	{ok, Mod:Func(Msg, State)}
    catch
	throw:R ->
	    {ok, R};
	error:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
	exit:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', R, {R, Stacktrace}}
    end.

try_handle_call(Mod, Msg, From, State) ->
    try
	{ok, Mod:handle_call(Msg, From, State)}
    catch
	throw:R ->
	    {ok, R};
	error:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
	exit:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', R, {R, Stacktrace}}
    end.

try_terminate(Mod, Reason, State) ->
    try
	{ok, Mod:terminate(Reason, State)}
    catch
	throw:R ->
	    {ok, R};
	error:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
	exit:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', R, {R, Stacktrace}}
    end.


%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod) ->
    Result = try_handle_call(Mod, Msg, From, State),
    case Result of
	{ok, {reply, Reply, NState}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, infinity, []);
	{ok, {reply, Reply, NState, Time1}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, Time1, []);
	{ok, {noreply, NState}} ->
	    loop(Parent, Name, NState, Mod, infinity, []);
	{ok, {noreply, NState, Time1}} ->
	    loop(Parent, Name, NState, Mod, Time1, []);
	{ok, {stop, Reason, Reply, NState}} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Name, Msg, Mod, NState, [])),
	    reply(From, Reply),
	    exit(R);
	Other -> handle_common_reply(Other, Parent, Name, Msg, Mod, State)
    end;
handle_msg(Msg, Parent, Name, State, Mod) ->
    Reply = try_dispatch(Msg, Mod, State),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State).

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, Debug) ->
    Result = try_handle_call(Mod, Msg, From, State),
    case Result of
	{ok, {reply, Reply, NState}} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, infinity, Debug1);
	{ok, {reply, Reply, NState, Time1}} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, Time1, Debug1);
	{ok, {noreply, NState}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, Debug1);
	{ok, {noreply, NState, Time1}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, Debug1);
	{ok, {stop, Reason, Reply, NState}} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Name, Msg, Mod, NState, Debug)),
	    _ = reply(Name, From, Reply, NState, Debug),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Parent, Name, Msg, Mod, State, Debug)
    end;
handle_msg(Msg, Parent, Name, State, Mod, Debug) ->
    Reply = try_dispatch(Msg, Mod, State),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug).

handle_common_reply(Reply, Parent, Name, Msg, Mod, State) ->
    case Reply of
	{ok, {noreply, NState}} ->
	    loop(Parent, Name, NState, Mod, infinity, []);
	{ok, {noreply, NState, Time1}} ->
	    loop(Parent, Name, NState, Mod, Time1, []);
	{ok, {stop, Reason, NState}} ->
	    terminate(Reason, Name, Msg, Mod, NState, []);
	{'EXIT', ExitReason, ReportReason} ->
	    terminate(ExitReason, ReportReason, Name, Msg, Mod, State, []);
	{ok, BadReply} ->
	    terminate({bad_return_value, BadReply}, Name, Msg, Mod, State, [])
    end.

handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug) ->
    case Reply of
	{ok, {noreply, NState}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, Debug1);
	{ok, {noreply, NState, Time1}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, Debug1);
	{ok, {stop, Reason, NState}} ->
	    terminate(Reason, Name, Msg, Mod, NState, Debug);
	{'EXIT', ExitReason, ReportReason} ->
	    terminate(ExitReason, ReportReason, Name, Msg, Mod, State, Debug);
	{ok, BadReply} ->
	    terminate({bad_return_value, BadReply}, Name, Msg, Mod, State, Debug)
    end.

reply(Name, {To, Tag}, Reply, State, Debug) ->
    reply({To, Tag}, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
		     {out, Reply, To, State} ).


%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [Name, State, Mod, Time]) ->
    loop(Parent, Name, State, Mod, Time, Debug).

-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time]) ->
    terminate(Reason, Name, [], Mod, State, Debug).

system_code_change([Name, State, Mod, Time], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of
	{ok, NewState} -> {ok, [Name, NewState, Mod, Time]};
	Else -> Else
    end.

system_get_state([_Name, State, _Mod, _Time]) ->
    {ok, State}.

system_replace_state(StateFun, [Name, State, Mod, Time]) ->
    NState = StateFun(State),
    {ok, NState, [Name, NState, Mod, Time]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{'$gen_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~p got call ~p from ~w~n",
		      [Name, Call, From]);
	{'$gen_cast', Cast} ->
	    io:format(Dev, "*DBG* ~p got cast ~p~n",
		      [Name, Cast]);
	_ ->
	    io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, To, State}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n", 
	      [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

-spec terminate(_, _, _, _, _, _) -> no_return().
terminate(Reason, Name, Msg, Mod, State, Debug) ->
    terminate(Reason, Reason, Name, Msg, Mod, State, Debug).

-spec terminate(_, _, _, _, _, _, _) -> no_return().
terminate(ExitReason, ReportReason, Name, Msg, Mod, State, Debug) ->
    Reply = try_terminate(Mod, ExitReason, State),
    case Reply of
	{'EXIT', ExitReason1, ReportReason1} ->
	    FmtState = format_status(terminate, Mod, get(), State),
	    error_info(ReportReason1, Name, Msg, FmtState, Debug),
	    exit(ExitReason1);
	_ ->
	    case ExitReason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		{shutdown,_}=Shutdown ->
		    exit(Shutdown);
		_ ->
		    FmtState = format_status(terminate, Mod, get(), State),
		    error_info(ReportReason, Name, Msg, FmtState, Debug),
		    exit(ExitReason)
	    end
    end.

error_info(_Reason, application_controller, _Msg, _State, _Debug) ->
    %% OTP-5811 Don't send an error report if it's the system process
    %% application_controller which is terminating - let init take care
    %% of it instead
    ok;
error_info(Reason, Name, Msg, State, Debug) ->
    Reason1 = 
	case Reason of
	    {undef,[{M,F,A,L}|MFAs]} ->
		case code:is_loaded(M) of
		    false ->
			{'module could not be loaded',[{M,F,A,L}|MFAs]};
		    _ ->
			case erlang:function_exported(M, F, length(A)) of
			    true ->
				Reason;
			    false ->
				{'function not exported',[{M,F,A,L}|MFAs]}
			end
		end;
	    _ ->
		Reason
	end,    
    format("** Generic server ~p terminating \n"
           "** Last message in was ~p~n"
           "** When Server state == ~p~n"
           "** Reason for termination == ~n** ~p~n",
	   [Name, Msg, State, Reason1]),
    sys:print_log(Debug),
    ok.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, State, Mod, _Time]] = StatusData,
    Header = gen:format_status_header("Status for generic server", Name),
    Log = sys:get_debug(log, Debug, []),
    Specfic = case format_status(Opt, Mod, PDict, State) of
		  S when is_list(S) -> S;
		  S -> [S]
	      end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", Log}]} |
     Specfic].

format_status(Opt, Mod, PDict, State) ->
    DefStatus = case Opt of
		    terminate -> State;
		    _ -> [{data, [{"State", State}]}]
		end,
    case erlang:function_exported(Mod, format_status, 2) of
	true ->
	    case catch Mod:format_status(Opt, [PDict, State]) of
		{'EXIT', _} -> DefStatus;
		Else -> Else
	    end;
	_ ->
	    DefStatus
    end.
