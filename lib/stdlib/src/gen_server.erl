%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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

%%%
%%% NOTE: If init_ack() return values are modified, see comment
%%%       above monitor_return() in gen.erl!
%%%

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
         start_monitor/3, start_monitor/4,
	 stop/1, stop/3,
	 call/2, call/3,
         send_request/2, wait_response/2,
         receive_response/2, check_response/2,
	 cast/2, reply/2,
	 abcast/2, abcast/3,
	 multi_call/2, multi_call/3, multi_call/4,
	 enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/6]).

%% System exports
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 system_get_state/1,
	 system_replace_state/2,
	 format_status/2]).

%% logger callback
-export([format_log/1, format_log/2]).

%% Internal exports
-export([init_it/6]).

-include("logger.hrl").

-define(
   STACKTRACE(),
   element(2, erlang:process_info(self(), current_stacktrace))).


-type server_ref() ::
        pid()
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-type request_id() :: term().

%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_continue(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
-callback format_status(Status) -> NewStatus when
      Status :: #{ state => term(),
                   message => term(),
                   reason => term(),
                   log => [sys:system_event()] },
      NewStatus :: Status.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-optional_callbacks(
    [handle_info/2, handle_continue/2, terminate/2, code_change/3,
     format_status/1, format_status/2]).

%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, term()} | {via, atom(), term()}
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

start_monitor(Mod, Args, Options) ->
    gen:start(?MODULE, monitor, Mod, Args, Options).

start_monitor(Name, Mod, Args, Options) ->
    gen:start(?MODULE, monitor, Name, Mod, Args, Options).


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
%% Send a request to a generic server and return a Key which should be
%% used with wait_response/2 or check_response/2 to fetch the
%% result of the request.

-spec send_request(Name::server_ref(), Request::term()) -> request_id().
send_request(Name, Request) ->
    gen:send_request(Name, '$gen_call', Request).

-spec wait_response(RequestId::request_id(), timeout()) ->
        {reply, Reply::term()} | 'timeout' | {error, {Reason::term(), server_ref()}}.
wait_response(RequestId, Timeout) ->
    gen:wait_response(RequestId, Timeout).

-spec receive_response(RequestId::request_id(), timeout()) ->
        {reply, Reply::term()} | 'timeout' | {error, {Reason::term(), server_ref()}}.
receive_response(RequestId, Timeout) ->
    gen:receive_response(RequestId, Timeout).

-spec check_response(Msg::term(), RequestId::request_id()) ->
        {reply, Reply::term()} | 'no_reply' | {error, {Reason::term(), server_ref()}}.
check_response(Msg, RequestId) ->
    gen:check_response(Msg, RequestId).

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
reply(From, Reply) ->
    gen:reply(From, Reply).

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
    HibernateAfterTimeout = gen:hibernate_after(Options),
    loop(Parent, Name, State, Mod, Timeout, HibernateAfterTimeout, Debug).

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
    HibernateAfterTimeout = gen:hibernate_after(Options),

    case init_it(Mod, Args) of
	{ok, {ok, State}} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, infinity, HibernateAfterTimeout, Debug);
	{ok, {ok, State, TimeoutHibernateOrContinue}} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, TimeoutHibernateOrContinue,
	         HibernateAfterTimeout, Debug);
	{ok, {stop, Reason}} ->
	    %% For consistency, we must make sure that the
	    %% registered name (if any) is unregistered before
	    %% the parent process is notified about the failure.
	    %% (Otherwise, the parent process could get
	    %% an 'already_started' error if it immediately
	    %% tried starting the process again.)
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	{ok, ignore} ->
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{ok, Else} ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error);
	{'EXIT', Class, Reason, Stacktrace} ->
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, terminate_reason(Class, Reason, Stacktrace)}),
	    erlang:raise(Class, Reason, Stacktrace)
    end.
init_it(Mod, Args) ->
    try
	{ok, Mod:init(Args)}
    catch
	throw:R -> {ok, R};
	Class:R:S -> {'EXIT', Class, R, S}
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------

loop(Parent, Name, State, Mod, {continue, Continue} = Msg, HibernateAfterTimeout, Debug) ->
    Reply = try_dispatch(Mod, handle_continue, Continue, State),
    case Debug of
	[] ->
	    handle_common_reply(Reply, Parent, Name, undefined, Msg, Mod,
				HibernateAfterTimeout, State);
	_ ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, Msg),
	    handle_common_reply(Reply, Parent, Name, undefined, Msg, Mod,
				HibernateAfterTimeout, State, Debug1)
    end;

loop(Parent, Name, State, Mod, hibernate, HibernateAfterTimeout, Debug) ->
    proc_lib:hibernate(?MODULE,wake_hib,[Parent, Name, State, Mod, HibernateAfterTimeout, Debug]);

loop(Parent, Name, State, Mod, infinity, HibernateAfterTimeout, Debug) ->
	receive
		Msg ->
			decode_msg(Msg, Parent, Name, State, Mod, infinity, HibernateAfterTimeout, Debug, false)
	after HibernateAfterTimeout ->
		loop(Parent, Name, State, Mod, hibernate, HibernateAfterTimeout, Debug)
	end;

loop(Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  timeout
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug, false).

wake_hib(Parent, Name, State, Mod, HibernateAfterTimeout, Debug) ->
    Msg = receive
	      Input ->
		  Input
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, hibernate, HibernateAfterTimeout, Debug, true).

decode_msg(Msg, Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug, Hib) ->
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, State, Mod, Time, HibernateAfterTimeout], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, ?STACKTRACE(), Name, undefined, Msg, Mod, State, Debug);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {in, Msg}),
	    handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout, Debug1)
    end.

%%% ---------------------------------------------------
%%% Send/receive functions
%%% ---------------------------------------------------
do_send(Dest, Msg) ->
    try erlang:send(Dest, Msg)
    catch
        error:_ -> ok
    end,
    ok.

do_multi_call([Node], Name, Req, infinity) when Node =:= node() ->
    % Special case when multi_call is used with local node only.
    % In that case we can leverage the benefit of recv_mark optimisation
    % existing in simple gen:call.
    try gen:call(Name, '$gen_call', Req, infinity) of
        {ok, Res} -> {[{Node, Res}],[]}
    catch exit:_ ->
        {[], [Node]}
    end;
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
%% {'EXIT', Class, Reason, Stack} (if an exception occurs)
%%
%% The Class, Reason and Stack are given to erlang:raise/3
%% to make sure proc_lib receives the proper reasons and
%% stacktraces.
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
        error:undef = R:Stacktrace when Func == handle_info ->
            case erlang:function_exported(Mod, handle_info, 2) of
                false ->
                    ?LOG_WARNING(
                       #{label=>{gen_server,no_handle_info},
                         module=>Mod,
                         message=>Msg},
                       #{domain=>[otp],
                         report_cb=>fun gen_server:format_log/2,
                         error_logger=>
                             #{tag=>warning_msg,
                               report_cb=>fun gen_server:format_log/1}}),
                    {ok, {noreply, State}};
                true ->
                    {'EXIT', error, R, Stacktrace}
            end;
	Class:R:Stacktrace ->
	    {'EXIT', Class, R, Stacktrace}
    end.

try_handle_call(Mod, Msg, From, State) ->
    try
	{ok, Mod:handle_call(Msg, From, State)}
    catch
	throw:R ->
	    {ok, R};
	Class:R:Stacktrace ->
	    {'EXIT', Class, R, Stacktrace}
    end.

try_terminate(Mod, Reason, State) ->
    case erlang:function_exported(Mod, terminate, 2) of
	true ->
	    try
		{ok, Mod:terminate(Reason, State)}
	    catch
		throw:R ->
		    {ok, R};
		Class:R:Stacktrace ->
		    {'EXIT', Class, R, Stacktrace}
	   end;
	false ->
	    {ok, ok}
    end.


%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, HibernateAfterTimeout) ->
    Result = try_handle_call(Mod, Msg, From, State),
    case Result of
	{ok, {reply, Reply, NState}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, []);
	{ok, {reply, Reply, NState, Time1}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, []);
	{ok, {noreply, NState}} ->
	    loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, []);
	{ok, {noreply, NState, Time1}} ->
	    loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, []);
	{ok, {stop, Reason, Reply, NState}} ->
	    try
		terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, [])
	    after
		reply(From, Reply)
	    end;
	Other -> handle_common_reply(Other, Parent, Name, From, Msg, Mod, HibernateAfterTimeout, State)
    end;
handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout) ->
    Reply = try_dispatch(Msg, Mod, State),
    handle_common_reply(Reply, Parent, Name, undefined, Msg, Mod, HibernateAfterTimeout, State).

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, HibernateAfterTimeout, Debug) ->
    Result = try_handle_call(Mod, Msg, From, State),
    case Result of
	{ok, {reply, Reply, NState}} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, Debug1);
	{ok, {reply, Reply, NState, Time1}} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, Debug1);
	{ok, {noreply, NState}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, Debug1);
	{ok, {noreply, NState, Time1}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, Debug1);
	{ok, {stop, Reason, Reply, NState}} ->
	    try
		terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, Debug)
	    after
		_ = reply(Name, From, Reply, NState, Debug)
	    end;
	Other ->
	    handle_common_reply(Other, Parent, Name, From, Msg, Mod, HibernateAfterTimeout, State, Debug)
    end;
handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout, Debug) ->
    Reply = try_dispatch(Msg, Mod, State),
    handle_common_reply(Reply, Parent, Name, undefined, Msg, Mod, HibernateAfterTimeout, State, Debug).

handle_common_reply(Reply, Parent, Name, From, Msg, Mod, HibernateAfterTimeout, State) ->
    case Reply of
	{ok, {noreply, NState}} ->
	    loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, []);
	{ok, {noreply, NState, Time1}} ->
	    loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, []);
	{ok, {stop, Reason, NState}} ->
	    terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, []);
	{'EXIT', Class, Reason, Stacktrace} ->
	    terminate(Class, Reason, Stacktrace, Name, From, Msg, Mod, State, []);
	{ok, BadReply} ->
	    terminate({bad_return_value, BadReply}, ?STACKTRACE(), Name, From, Msg, Mod, State, [])
    end.

handle_common_reply(Reply, Parent, Name, From, Msg, Mod, HibernateAfterTimeout, State, Debug) ->
    case Reply of
	{ok, {noreply, NState}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, Debug1);
	{ok, {noreply, NState, Time1}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, Debug1);
	{ok, {stop, Reason, NState}} ->
	    terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, Debug);
	{'EXIT', Class, Reason, Stacktrace} ->
	    terminate(Class, Reason, Stacktrace, Name, From, Msg, Mod, State, Debug);
	{ok, BadReply} ->
	    terminate({bad_return_value, BadReply}, ?STACKTRACE(), Name, From, Msg, Mod, State, Debug)
    end.

reply(Name, From, Reply, State, Debug) ->
    reply(From, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
		     {out, Reply, From, State} ).


%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [Name, State, Mod, Time, HibernateAfterTimeout]) ->
    loop(Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug).

-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time, _HibernateAfterTimeout]) ->
    terminate(Reason, ?STACKTRACE(), Name, undefined, [], Mod, State, Debug).

system_code_change([Name, State, Mod, Time, HibernateAfterTimeout], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of
	{ok, NewState} -> {ok, [Name, NewState, Mod, Time, HibernateAfterTimeout]};
	Else -> Else
    end.

system_get_state([_Name, State, _Mod, _Time, _HibernateAfterTimeout]) ->
    {ok, State}.

system_replace_state(StateFun, [Name, State, Mod, Time, HibernateAfterTimeout]) ->
    NState = StateFun(State),
    {ok, NState, [Name, NState, Mod, Time, HibernateAfterTimeout]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{'$gen_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~tp got call ~tp from ~tw~n",
		      [Name, Call, From]);
	{'$gen_cast', Cast} ->
	    io:format(Dev, "*DBG* ~tp got cast ~tp~n",
		      [Name, Cast]);
	_ ->
	    io:format(Dev, "*DBG* ~tp got ~tp~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, {To,_Tag}, State}, Name) ->
    io:format(Dev, "*DBG* ~tp sent ~tp to ~tw, new state ~tp~n",
	      [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~tp new state ~tp~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~tp dbg  ~tp~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%%
%%% terminate/8 is triggered by {stop, Reason} or bad
%%% return values. The stacktrace is generated via the
%%% ?STACKTRACE() macro and the ReportReason must not
%%% be wrapped in tuples.
%%%
%%% terminate/9 is triggered in case of error/exit in
%%% the user callback. In this case the report reason
%%% always includes the user stacktrace.
%%%
%%% The reason received in the terminate/2 callbacks
%%% always includes the stacktrace for errors and never
%%% for exits.
%%% ---------------------------------------------------

-spec terminate(_, _, _, _, _, _, _, _) -> no_return().
terminate(Reason, Stacktrace, Name, From, Msg, Mod, State, Debug) ->
  terminate(exit, Reason, Stacktrace, false, Name, From, Msg, Mod, State, Debug).

-spec terminate(_, _, _, _, _, _, _, _, _) -> no_return().
terminate(Class, Reason, Stacktrace, Name, From, Msg, Mod, State, Debug) ->
  terminate(Class, Reason, Stacktrace, true, Name, From, Msg, Mod, State, Debug).

-spec terminate(_, _, _, _, _, _, _, _, _, _) -> no_return().
terminate(Class, Reason, Stacktrace, ReportStacktrace, Name, From, Msg, Mod, State, Debug) ->
    Reply = try_terminate(Mod, terminate_reason(Class, Reason, Stacktrace), State),
    case Reply of
	{'EXIT', C, R, S} ->
	    error_info(R, S, Name, From, Msg, Mod, State, Debug),
	    erlang:raise(C, R, S);
	_ ->
	    case {Class, Reason} of
		{exit, normal} -> ok;
		{exit, shutdown} -> ok;
		{exit, {shutdown,_}} -> ok;
		_ when ReportStacktrace ->
		    error_info(Reason, Stacktrace, Name, From, Msg, Mod, State, Debug);
                _ ->
		    error_info(Reason, undefined, Name, From, Msg, Mod, State, Debug)
	    end
    end,
    case Stacktrace of
	[] ->
	    erlang:Class(Reason);
	_ ->
	    erlang:raise(Class, Reason, Stacktrace)
    end.

terminate_reason(error, Reason, Stacktrace) -> {Reason, Stacktrace};
terminate_reason(exit, Reason, _Stacktrace) -> Reason.

error_info(_Reason, _ST, application_controller, _From, _Msg, _Mod, _State, _Debug) ->
    %% OTP-5811 Don't send an error report if it's the system process
    %% application_controller which is terminating - let init take care
    %% of it instead
    ok;
error_info(Reason, ST, Name, From, Msg, Mod, State, Debug) ->
    Log = sys:get_log(Debug),
    Status =
        gen:format_status(Mod, terminate,
                          #{ reason => Reason,
                             state => State,
                             message => Msg,
                             log => Log },
                          [get(),State]),
    ReportReason =
        if ST == undefined ->
                %% When ST is undefined, it should not be included in the
                %% reported reason for the crash as it is then caused
                %% by an invalid return from a callback and thus thus the
                %% stacktrace is irrelevant.
                maps:get(reason, Status);
           true ->
                {maps:get(reason, Status), ST}
        end,

    ?LOG_ERROR(#{label=>{gen_server,terminate},
                 name=>Name,
                 last_message=>maps:get(message,Status),
                 state=>maps:get('EXIT',Status,maps:get('$status',Status,maps:get(state,Status))),
                 log=>format_log_state(Mod,maps:get(log,Status)),
                 reason=>ReportReason,
                 client_info=>client_stacktrace(From)},
               #{domain=>[otp],
                 report_cb=>fun gen_server:format_log/2,
                 error_logger=>#{tag=>error,
                                 report_cb=>fun gen_server:format_log/1}}),
    ok.

client_stacktrace(undefined) ->
    undefined;
client_stacktrace({From,_Tag}) ->
    client_stacktrace(From);
client_stacktrace(From) when is_pid(From), node(From) =:= node() ->
    case process_info(From, [current_stacktrace, registered_name]) of
        undefined ->
            {From,dead};
        [{current_stacktrace, Stacktrace}, {registered_name, []}]  ->
            {From,{From,Stacktrace}};
        [{current_stacktrace, Stacktrace}, {registered_name, Name}]  ->
            {From,{Name,Stacktrace}}
    end;
client_stacktrace(From) when is_pid(From) ->
    {From,remote}.


%% format_log/1 is the report callback used by Logger handler
%% error_logger only. It is kept for backwards compatibility with
%% legacy error_logger event handlers. This function must always
%% return {Format,Args} compatible with the arguments in this module's
%% calls to error_logger prior to OTP-21.0.
format_log(Report) ->
    Depth = error_logger:get_format_depth(),
    FormatOpts = #{chars_limit => unlimited,
                   depth => Depth,
                   single_line => false,
                   encoding => utf8},
    format_log_multi(limit_report(Report,Depth),FormatOpts).

limit_report(Report,unlimited) ->
    Report;
limit_report(#{label:={gen_server,terminate},
               last_message:=Msg,
               state:=State,
               log:=Log,
               reason:=Reason,
               client_info:=Client}=Report,
            Depth) ->
    Report#{last_message=>io_lib:limit_term(Msg,Depth),
            state=>io_lib:limit_term(State,Depth),
            log=>[io_lib:limit_term(L,Depth)||L<-Log],
            reason=>io_lib:limit_term(Reason,Depth),
            client_info=>limit_client_report(Client,Depth)};
limit_report(#{label:={gen_server,no_handle_info},
               message:=Msg}=Report,Depth) ->
    Report#{message=>io_lib:limit_term(Msg,Depth)}.

limit_client_report({From,{Name,Stacktrace}},Depth) ->
    {From,{Name,io_lib:limit_term(Stacktrace,Depth)}};
limit_client_report(Client,_) ->
    Client.

%% format_log/2 is the report callback for any Logger handler, except
%% error_logger.
format_log(Report, FormatOpts0) ->
    Default = #{chars_limit => unlimited,
                depth => unlimited,
                single_line => false,
                encoding => utf8},
    FormatOpts = maps:merge(Default,FormatOpts0),
    IoOpts =
        case FormatOpts of
            #{chars_limit:=unlimited} ->
                [];
            #{chars_limit:=Limit} ->
                [{chars_limit,Limit}]
        end,
    {Format,Args} = format_log_single(Report, FormatOpts),
    io_lib:format(Format, Args, IoOpts).

format_log_single(#{label:={gen_server,terminate},
                    name:=Name,
                    last_message:=Msg,
                    state:=State,
                    log:=Log,
                    reason:=Reason,
                    client_info:=Client},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format1 = lists:append(["Generic server ",P," terminating. Reason: ",P,
                            ". Last message: ", P, ". State: ",P,"."]),
    {ServerLogFormat,ServerLogArgs} = format_server_log_single(Log,FormatOpts),
    {ClientLogFormat,ClientLogArgs} = format_client_log_single(Client,FormatOpts),

    Args1 =
        case Depth of
            unlimited ->
                [Name,fix_reason(Reason),Msg,State];
            _ ->
                [Name,Depth,fix_reason(Reason),Depth,Msg,Depth,State,Depth]
        end,
    {Format1++ServerLogFormat++ClientLogFormat,
     Args1++ServerLogArgs++ClientLogArgs};
format_log_single(#{label:={gen_server,no_handle_info},
                    module:=Mod,
                    message:=Msg},
                  #{single_line:=true,depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format = lists:append(["Undefined handle_info in ",P,
                           ". Unhandled message: ",P,"."]),
    Args =
        case Depth of
            unlimited ->
                [Mod,Msg];
            _ ->
                [Mod,Depth,Msg,Depth]
        end,
    {Format,Args};
format_log_single(Report,FormatOpts) ->
    format_log_multi(Report,FormatOpts).

format_log_multi(#{label:={gen_server,terminate},
                   name:=Name,
                   last_message:=Msg,
                   state:=State,
                   log:=Log,
                   reason:=Reason,
                   client_info:=Client},
                 #{depth:=Depth}=FormatOpts) ->
    Reason1 = fix_reason(Reason),
    {ClientFmt,ClientArgs} = format_client_log(Client,FormatOpts),
    P = p(FormatOpts),
    Format =
        lists:append(
          ["** Generic server ",P," terminating \n"
           "** Last message in was ",P,"~n"
           "** When Server state == ",P,"~n"
           "** Reason for termination ==~n** ",P,"~n"] ++
               case Log of
                   [] -> [];
                   _ -> ["** Log ==~n** ["|
                         lists:join(",~n    ",lists:duplicate(length(Log),P))]++
                            ["]~n"]
               end) ++ ClientFmt,
    Args =
        case Depth of
            unlimited ->
                [Name, Msg, State, Reason1] ++ Log ++ ClientArgs;
            _ ->
                [Name, Depth, Msg, Depth, State, Depth, Reason1, Depth] ++
                    case Log of
                        [] -> [];
                        _ -> lists:flatmap(fun(L) -> [L, Depth] end, Log)
                    end ++ ClientArgs
        end,
    {Format,Args};
format_log_multi(#{label:={gen_server,no_handle_info},
                   module:=Mod,
                   message:=Msg},
                 #{depth:=Depth}=FormatOpts) ->
    P = p(FormatOpts),
    Format =
        "** Undefined handle_info in ~p~n"
        "** Unhandled message: "++P++"~n",
    Args =
        case Depth of
            unlimited ->
                [Mod,Msg];
            _ ->
                [Mod,Msg,Depth]
                    end,
    {Format,Args}.

fix_reason({undef,[{M,F,A,L}|MFAs]}=Reason) ->
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
fix_reason(Reason) ->
    Reason.

format_server_log_single([],_) ->
    {"",[]};
format_server_log_single(Log,FormatOpts) ->
    Args =
        case maps:get(depth,FormatOpts) of
            unlimited ->
                [Log];
            Depth ->
                [Log, Depth]
        end,
     {" Log: "++p(FormatOpts),Args}.

format_client_log_single(undefined,_) ->
    {"",[]};
format_client_log_single({From,dead},_) ->
    {" Client ~0p is dead.",[From]};
format_client_log_single({From,remote},_) ->
    {" Client ~0p is remote on node ~0p.", [From, node(From)]};
format_client_log_single({_From,{Name,Stacktrace0}},FormatOpts) ->
    P = p(FormatOpts),
    %% Minimize the stacktrace a bit for single line reports. This is
    %% hopefully enough to point out the position.
    Stacktrace = lists:sublist(Stacktrace0,4),
    Args =
        case maps:get(depth,FormatOpts) of
            unlimited ->
                [Name, Stacktrace];
            Depth ->
                [Name, Depth, Stacktrace, Depth]
        end,
    {" Client "++P++" stacktrace: "++P++".", Args}.

format_client_log(undefined,_) ->
    {"", []};
format_client_log({From,dead},_) ->
    {"** Client ~p is dead~n", [From]};
format_client_log({From,remote},_) ->
    {"** Client ~p is remote on node ~p~n", [From, node(From)]};
format_client_log({_From,{Name,Stacktrace}},FormatOpts) ->
    P = p(FormatOpts),
    Format = lists:append(["** Client ",P," stacktrace~n",
                           "** ",P,"~n"]),
    Args =
        case maps:get(depth,FormatOpts) of
            unlimited ->
                [Name, Stacktrace];
            Depth ->
                [Name, Depth, Stacktrace, Depth]
        end,
    {Format,Args}.

p(#{single_line:=Single,depth:=Depth,encoding:=Enc}) ->
    "~"++single(Single)++mod(Enc)++p(Depth);
p(unlimited) ->
    "p";
p(_Depth) ->
    "P".

single(true) -> "0";
single(false) -> "".

mod(latin1) -> "";
mod(_) -> "t".

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, State, Mod, _Time, _HibernateAfterTimeout]] = StatusData,
    Header = gen:format_status_header("Status for generic server", Name),
    Status =
        case gen:format_status(Mod, Opt, #{ state => State, log => sys:get_log(Debug) },
                               [PDict, State]) of
            #{ 'EXIT' := R } = M ->
                M#{ '$status' => [{data,[{"State",R}]}] };
            %% Status is set when the old format_status/2 is called,
            %% so we do a little backwards compatibility dance here
            #{ '$status' := S } = M when is_list(S) -> M;
            #{ '$status' := S } = M -> M#{ '$status' := [S] };
            #{ state := S } = M ->
                M#{ '$status' => [{data, [{"State",S}] }] }
        end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", format_log_state(Mod, maps:get(log,Status))}]} |
     maps:get('$status',Status)].

format_log_state(Mod, Log) ->
    %% If format_status/1 was exported, the log has already been handled by
    %% that call, so we should not pass all log events into the callback again.
    case erlang:function_exported(Mod, format_status, 1) of
        false ->
            [case Event of
                 {out,Msg,From,State} ->
                     Status = gen:format_status(
                                Mod, terminate, #{ state => State },
                                [get(), State]),
                     {out, Msg, From, maps:get(state, Status) };
                 {noreply,State} ->
                     Status = gen:format_status(
                                Mod, terminate, #{ state => State },
                                [get(), State]),
                     {noreply, maps:get(state, Status)};
                 _ -> Event
             end || Event <- Log];
        true ->
            Log
    end.
