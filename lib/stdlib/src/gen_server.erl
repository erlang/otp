%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
         send_request/2, send_request/4,
         wait_response/2, receive_response/2, check_response/2,
         wait_response/3, receive_response/3, check_response/3,
         reqids_new/0, reqids_size/1,
         reqids_add/3, reqids_to_list/1,
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

-export_type(
   [from/0,
    reply_tag/0,
    request_id/0,
    request_id_collection/0,
    format_status/0]).

-export_type(
   [server_name/0,
    server_ref/0,
    start_opt/0,
    enter_loop_opt/0,
    start_ret/0,
    start_mon_ret/0]).

-define(
   STACKTRACE(),
   element(2, erlang:process_info(self(), current_stacktrace))).

-define(
	is_timeout(X),
	( (X) =:= infinity orelse ( is_integer(X) andalso (X) >= 0 ) )
).

-record(callback_cache,{module :: module(),
                        handle_call :: fun((Request :: term(), From :: from(), State :: term()) ->
                            {reply, Reply :: term(), NewState :: term()} |
                            {reply, Reply :: term(), NewState :: term(), timeout() | hibernate | {continue, term()}} |
                            {noreply, NewState :: term()} |
                            {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
                            {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                            {stop, Reason :: term(), NewState :: term()}),
                        handle_cast :: fun((Request :: term(), State :: term()) ->
                            {noreply, NewState :: term()} |
                            {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
                            {stop, Reason :: term(), NewState :: term()}),
                        handle_info :: fun((Info :: timeout | term(), State :: term()) ->
                            {noreply, NewState :: term()} |
                            {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
                            {stop, Reason :: term(), NewState :: term()})}).
%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore | {error, Reason :: term()}.
-callback handle_call(Request :: term(), From :: from(),
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
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().
-type format_status() ::
        #{ state => term(),
           message => term(),
           reason => term(),
           log => [sys:system_event()] }.
-callback format_status(Status) -> NewStatus when
      Status    :: format_status(),
      NewStatus :: format_status().

-optional_callbacks(
    [handle_info/2, handle_continue/2, terminate/2, code_change/3,
     format_status/1, format_status/2]).



-type from() ::	{Client :: pid(), Tag :: reply_tag()}.
-opaque reply_tag() :: gen:reply_tag().

-opaque request_id() :: gen:request_id().

-opaque request_id_collection() :: gen:request_id_collection().

-type response_timeout() ::
        timeout() | {abs, integer()}.

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

-type server_name() :: % Duplicate of gen:emgr_name()
        {'local', LocalName :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-type server_ref() :: % What gen:call/3,4 and gen:stop/1,3 accepts
        pid()
      | (LocalName :: atom())
      | {Name :: atom(), Node :: atom()}
      | {'global', GlobalName :: term()}
      | {'via', RegMod :: module(), ViaName :: term()}.

-type start_opt() :: % Duplicate of gen:option()
        {'timeout', Timeout :: timeout()}
      | {'spawn_opt', SpawnOptions :: [proc_lib:spawn_option()]}
      | enter_loop_opt().
%%
-type enter_loop_opt() :: % Some gen:option()s works for enter_loop/*
	{'hibernate_after', HibernateAfterTimeout :: timeout()}
      | {'debug', Dbgs :: [sys:debug_option()]}.

-type start_ret() :: % gen:start_ret() without monitor return
        {'ok', Pid :: pid()}
      | 'ignore'
      | {'error', Reason :: term()}.

-type start_mon_ret() :: % gen:start_ret() with only monitor return
        {'ok', {Pid :: pid(), MonRef :: reference()}}
      | 'ignore'
      | {'error', Reason :: term()}.

%%% ---------------------------------------------------

-spec start(
	Module  :: module(),
        Args    :: term(),
        Options :: [start_opt()]
       ) ->
		   start_ret().
%%
start(Module, Args, Options)
  when is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, nolink, Module, Args, Options);
start(Module, Args, Options) ->
    error(badarg, [Module, Args, Options]).

-spec start(
	ServerName :: server_name(),
	Module     :: module(),
        Args       :: term(),
        Options    :: [start_opt()]
       ) ->
		   start_ret().
%%
start(ServerName, Module, Args, Options)
  when is_tuple(ServerName), is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, nolink, ServerName, Module, Args, Options);
start(ServerName, Module, Args, Options) ->
    error(badarg, [ServerName, Module, Args, Options]).

-spec start_link(
	Module  :: module(),
        Args    :: term(),
        Options :: [start_opt()]
       ) ->
		   start_ret().
%%
start_link(Module, Args, Options)
  when is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, link, Module, Args, Options);
start_link(Module, Args, Options) ->
    error(badarg, [Module, Args, Options]).

-spec start_link(
	ServerName :: server_name(),
	Module     :: module(),
        Args       :: term(),
        Options    :: [start_opt()]
       ) ->
		   start_ret().
%%
start_link(ServerName, Module, Args, Options)
  when is_tuple(ServerName), is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, link, ServerName, Module, Args, Options);
start_link(ServerName, Module, Args, Options) ->
    error(badarg, [ServerName, Module, Args, Options]).

-spec start_monitor(
	Module  :: module(),
        Args    :: term(),
        Options :: [start_opt()]
       ) ->
		   start_mon_ret().
%%
start_monitor(Module, Args, Options)
  when is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, monitor, Module, Args, Options);
start_monitor(Module, Args, Options) ->
    error(badarg, [Module, Args, Options]).

-spec start_monitor(
	ServerName :: server_name(),
	Module     :: module(),
        Args       :: term(),
        Options    :: [start_opt()]
       ) ->
		   start_mon_ret().
%%
start_monitor(ServerName, Module, Args, Options)
  when is_tuple(ServerName), is_atom(Module), is_list(Options) ->
    gen:start(?MODULE, monitor, ServerName, Module, Args, Options);
start_monitor(ServerName, Module, Args, Options) ->
    error(badarg, [ServerName, Module, Args, Options]).


%% -----------------------------------------------------------------
%% Stop a generic server and wait for it to terminate.
%% If the server is located at another node, that node will
%% be monitored.
%% -----------------------------------------------------------------

-spec stop(
        ServerRef :: server_ref()
       ) -> ok.
%%
stop(ServerRef) ->
    gen:stop(ServerRef).

-spec stop(
	ServerRef :: server_ref(),
	Reason    :: term(),
	Timeout   :: timeout()
       ) -> ok.
%%
stop(ServerRef, Reason, Timeout) ->
    gen:stop(ServerRef, Reason, Timeout).

%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on timeouts) ?).
%% ----------------------------------------------------------------- 

-spec call(
        ServerRef :: server_ref(),
        Request   :: term()
       ) ->
                  Reply :: term().
%%
call(ServerRef, Request) ->
    case catch gen:call(ServerRef, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [ServerRef, Request]}})
    end.

-spec call(
        ServerRef :: server_ref(),
        Request   :: term(),
        Timeout   :: timeout()
       ) ->
                  Reply :: term().
%%
call(ServerRef, Request, Timeout) ->
    case catch gen:call(ServerRef, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [ServerRef, Request, Timeout]}})
    end.

%% -----------------------------------------------------------------
%% Send a request to a generic server and return a Key which should be
%% used with wait_response/2 or check_response/2 to fetch the
%% result of the request.

-spec send_request(ServerRef::server_ref(), Request::term()) ->
          ReqId::request_id().

send_request(ServerRef, Request) ->
    try
        gen:send_request(ServerRef, '$gen_call', Request)
    catch
        error:badarg ->
            error(badarg, [ServerRef, Request])
    end.

-spec send_request(ServerRef::server_ref(),
                   Request::term(),
                   Label::term(),
                   ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().

send_request(ServerRef, Request, Label, ReqIdCol) ->
    try
        gen:send_request(ServerRef, '$gen_call', Request, Label, ReqIdCol)
    catch
        error:badarg ->
            error(badarg, [ServerRef, Request, Label, ReqIdCol])
    end.

-spec wait_response(ReqId, WaitTime) -> Result when
      ReqId :: request_id(),
      WaitTime :: response_timeout(),
      Response :: {reply, Reply::term()}
                | {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

wait_response(ReqId, WaitTime) ->
    try
        gen:wait_response(ReqId, WaitTime)
    catch
        error:badarg ->
            error(badarg, [ReqId, WaitTime])
    end.

-spec wait_response(ReqIdCollection, WaitTime, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      WaitTime :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

wait_response(ReqIdCol, WaitTime, Delete) ->
    try
        gen:wait_response(ReqIdCol, WaitTime, Delete)
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, WaitTime, Delete])
    end.

-spec receive_response(ReqId, Timeout) -> Result when
      ReqId :: request_id(),
      Timeout :: response_timeout(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'timeout'.

receive_response(ReqId, Timeout) ->
    try
        gen:receive_response(ReqId, Timeout)
    catch
        error:badarg ->
            error(badarg, [ReqId, Timeout])
    end.

-spec receive_response(ReqIdCollection, Timeout, Delete) -> Result when
      ReqIdCollection :: request_id_collection(),
      Timeout :: response_timeout(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'timeout'.

receive_response(ReqIdCol, Timeout, Delete) ->
    try
        gen:receive_response(ReqIdCol, Timeout, Delete)
    catch
        error:badarg ->
            error(badarg, [ReqIdCol, Timeout, Delete])
    end.

-spec check_response(Msg, ReqId) -> Result when
      Msg :: term(),
      ReqId :: request_id(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: Response | 'no_reply'.

check_response(Msg, ReqId) ->
    try
        gen:check_response(Msg, ReqId)
    catch
        error:badarg ->
            error(badarg, [Msg, ReqId])
    end.

-spec check_response(Msg, ReqIdCollection, Delete) -> Result when
      Msg :: term(),
      ReqIdCollection :: request_id_collection(),
      Delete :: boolean(),
      Response :: {reply, Reply::term()} |
                  {error, {Reason::term(), server_ref()}},
      Result :: {Response,
                 Label::term(),
                 NewReqIdCollection::request_id_collection()} |
                'no_request' |
                'no_reply'.

check_response(Msg, ReqIdCol, Delete) ->
    try
        gen:check_response(Msg, ReqIdCol, Delete)
    catch
        error:badarg ->
            error(badarg, [Msg, ReqIdCol, Delete])
    end.

-spec reqids_new() ->
          NewReqIdCollection::request_id_collection().

reqids_new() ->
    gen:reqids_new().

-spec reqids_size(ReqIdCollection::request_id_collection()) ->
          non_neg_integer().

reqids_size(ReqIdCollection) ->
    try
        gen:reqids_size(ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqIdCollection])
    end.

-spec reqids_add(ReqId::request_id(), Label::term(),
                 ReqIdCollection::request_id_collection()) ->
          NewReqIdCollection::request_id_collection().

reqids_add(ReqId, Label, ReqIdCollection) ->
    try
        gen:reqids_add(ReqId, Label, ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqId, Label, ReqIdCollection])
    end.

-spec reqids_to_list(ReqIdCollection::request_id_collection()) ->
          [{ReqId::request_id(), Label::term()}].

reqids_to_list(ReqIdCollection) ->
    try
        gen:reqids_to_list(ReqIdCollection)
    catch
        error:badarg -> error(badarg, [ReqIdCollection])
    end.

%% -----------------------------------------------------------------
%% Make a cast to a generic server.
%% -----------------------------------------------------------------

-spec cast(
        ServerRef :: server_ref(),
        Request   :: term()
       ) ->
                  ok.
%%
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

-spec reply(
        Client :: from(),
        Reply  :: term()
       ) ->
                   ok.
%%
reply(Client, Reply) ->
    gen:reply(Client, Reply).

%% ----------------------------------------------------------------- 
%% Asynchronous broadcast, returns nothing, it's just send 'n' pray
%%-----------------------------------------------------------------  

-spec abcast(
        Name    :: atom(),
        Request :: term()
       ) ->
                  abcast.
%%
abcast(Name, Request) when is_atom(Name) ->
    do_abcast([node() | nodes()], Name, cast_msg(Request)).

-spec abcast(
        Nodes   :: [node()],
        Name    :: atom(),
        Request :: term()
       ) ->
                  abcast.
%%
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

-spec multi_call(
        Name    :: atom(),
        Request :: term()
       ) ->
                        {Replies ::
                           [{Node :: node(), Reply :: term()}],
                         BadNodes :: [node()]
                        }.
%%
multi_call(Name, Request)
  when is_atom(Name) ->
    multi_call([node() | nodes()], Name, Request, infinity).

-spec multi_call(
        Nodes   :: [node()],
        Name    :: atom(),
        Request :: term()
       ) ->
                        {Replies ::
                           [{Node :: node(), Reply :: term()}],
                         BadNodes :: [node()]
                        }.
%%
multi_call(Nodes, Name, Request)
  when is_list(Nodes), is_atom(Name) ->
    multi_call(Nodes, Name, Request, infinity).

-spec multi_call(
        Nodes   :: [node()],
        Name    :: atom(),
        Request :: term(),
        Timeout :: timeout()
       ) ->
                        {Replies ::
                           [{Node :: node(), Reply :: term()}],
                         BadNodes :: [node()]
                        }.
%%
multi_call(Nodes, Name, Request, Timeout)
  when is_list(Nodes), is_atom(Name), ?is_timeout(Timeout) ->
    Alias = alias(),
    try
        Timer = if Timeout == infinity -> undefined;
                   true -> erlang:start_timer(Timeout, self(), Alias)
                end,
        Reqs = mc_send(Nodes, Name, Alias, Request, Timer, []),
        mc_recv(Reqs, Alias, Timer, [], [])
    after
        _ = unalias(Alias)
    end.

-dialyzer({no_improper_lists, mc_send/6}).

mc_send([], _Name, _Alias, _Request, _Timer, Reqs) ->
    Reqs;
mc_send([Node|Nodes], Name, Alias, Request, Timer, Reqs) when is_atom(Node) ->
    NN = {Name, Node},
    Mon = try
              erlang:monitor(process, NN, [{tag, Alias}])
          catch
              error:badarg ->
                  %% Node not alive...
                  M = make_ref(),
                  Alias ! {Alias, M, process, NN, noconnection},
                  M
          end,
    try
        %% We use 'noconnect' since it is no point in bringing up a new
        %% connection if it was not brought up by the monitor signal...
        _ = erlang:send(NN,
                        {'$gen_call', {self(), [[alias|Alias]|Mon]}, Request},
                        [noconnect]),
        ok
    catch
        _:_ ->
            ok
    end,
    mc_send(Nodes, Name, Alias, Request, Timer, [[Node|Mon]|Reqs]);
mc_send(_BadNodes, _Name, Alias, _Request, Timer, Reqs) ->
    %% Cleanup then fail...
    unalias(Alias),
    mc_cancel_timer(Timer, Alias),
    _ = mc_recv_tmo(Reqs, Alias, [], []),
    error(badarg).

mc_recv([], Alias, Timer, Replies, BadNodes) ->
    mc_cancel_timer(Timer, Alias),
    unalias(Alias),
    {Replies, BadNodes};
mc_recv([[Node|Mon] | RestReqs] = Reqs, Alias, Timer, Replies, BadNodes) ->
    receive
        {[[alias|Alias]|Mon], Reply} ->
            erlang:demonitor(Mon, [flush]),
            mc_recv(RestReqs, Alias, Timer, [{Node,Reply}|Replies], BadNodes);
        {Alias, Mon, process, _, _} ->
            mc_recv(RestReqs, Alias, Timer, Replies, [Node|BadNodes]);
        {timeout, Timer, Alias} ->
            unalias(Alias),
            mc_recv_tmo(Reqs, Alias, Replies, BadNodes)
    end.

mc_recv_tmo([], _Alias, Replies, BadNodes) ->
    {Replies, BadNodes};
mc_recv_tmo([[Node|Mon] | RestReqs], Alias, Replies, BadNodes) ->
    erlang:demonitor(Mon),
    receive
        {[[alias|Alias]|Mon], Reply} ->
            mc_recv_tmo(RestReqs, Alias, [{Node,Reply}|Replies], BadNodes);
        {Alias, Mon, process, _, _} ->
            mc_recv_tmo(RestReqs, Alias, Replies, [Node|BadNodes])
    after
        0 ->
            mc_recv_tmo(RestReqs, Alias, Replies, [Node|BadNodes])
    end.

mc_cancel_timer(undefined, _Alias) ->
    ok;
mc_cancel_timer(Timer, Alias) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Timer, Alias} ->
                    ok
            end;
        _ ->
            ok
    end.

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

-spec enter_loop(
        Module  :: module(),
        Options :: [enter_loop_opt()],
        State   :: term()
       ) ->
                        no_return().
%%
enter_loop(Mod, Options, State)
  when is_atom(Mod), is_list(Options) ->
    enter_loop(Mod, Options, State, self(), infinity).

-spec enter_loop(
        Module     :: module(),
        Options    :: [enter_loop_opt()],
        State      :: term(),
        ServerName :: server_name() | pid()
       ) ->
                        no_return();
       (
         Module  :: module(),
         Options :: [enter_loop_opt()],
         State   :: term(),
         Timeout :: timeout()
       ) ->
                        no_return();
       (
           Module    :: module(),
           Options   :: [enter_loop_opt()],
           State     :: term(),
           Hibernate :: 'hibernate'
       ) ->
                        no_return();
       (
           Module  :: module(),
           Options :: [enter_loop_opt()],
           State   :: term(),
           Cont    :: {'continue', term()}
       ) ->
                        no_return().
%%
enter_loop(Mod, Options, State, ServerName = {Scope, _})
  when is_atom(Mod), is_list(Options), Scope == local;
       is_atom(Mod), is_list(Options), Scope == global ->
    enter_loop(Mod, Options, State, ServerName, infinity);
%%
enter_loop(Mod, Options, State, ServerName = {via, _, _})
  when is_atom(Mod), is_list(Options) ->
    enter_loop(Mod, Options, State, ServerName, infinity);
%%
enter_loop(Mod, Options, State, TimeoutOrHibernate)
  when is_atom(Mod), is_list(Options), ?is_timeout(TimeoutOrHibernate);
       is_atom(Mod), is_list(Options), TimeoutOrHibernate =:= hibernate ->
    enter_loop(Mod, Options, State, self(), TimeoutOrHibernate);
%%
enter_loop(Mod, Options, State, {continue, _}=Continue)
  when is_atom(Mod), is_list(Options) ->
    enter_loop(Mod, Options, State, self(), Continue).

-spec enter_loop(
        Module     :: module(),
        Options    :: [enter_loop_opt()],
        State      :: term(),
        ServerName :: server_name() | pid(),
        Timeout    :: timeout()
       ) ->
                        no_return();
       (
           Module     :: module(),
           Options    :: [enter_loop_opt()],
           State      :: term(),
           ServerName :: server_name() | pid(),
           Hibernate  :: 'hibernate'
       ) ->
                        no_return();
       (
           Module     :: module(),
           Options    :: [enter_loop_opt()],
           State      :: term(),
           ServerName :: server_name() | pid(),
           Cont       :: {'continue', term()}
       ) ->
                        no_return().
%%
enter_loop(Mod, Options, State, ServerName, TimeoutOrHibernate)
  when is_atom(Mod), is_list(Options), ?is_timeout(TimeoutOrHibernate);
       is_atom(Mod), is_list(Options), TimeoutOrHibernate =:= hibernate ->
    Name = gen:get_proc_name(ServerName),
    Parent = gen:get_parent(),
    Debug = gen:debug_options(Name, Options),
    HibernateAfterTimeout = gen:hibernate_after(Options),
    CbCache = create_callback_cache(Mod),
    loop(Parent, Name, State, CbCache, TimeoutOrHibernate, HibernateAfterTimeout, Debug);
%%
enter_loop(Mod, Options, State, ServerName, {continue, _}=Continue)
  when is_atom(Mod), is_list(Options) ->
    Name = gen:get_proc_name(ServerName),
    Parent = gen:get_parent(),
    Debug = gen:debug_options(Name, Options),
    HibernateAfterTimeout = gen:hibernate_after(Options),
    CbCache = create_callback_cache(Mod),
    loop(Parent, Name, State, CbCache, Continue, HibernateAfterTimeout, Debug).

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
    CbCache = create_callback_cache(Mod),
    case init_it(Mod, Args) of
	{ok, {ok, State}} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(
              Parent, Name, State, CbCache, infinity,
              HibernateAfterTimeout, Debug);
        {ok, {ok, State, TimeoutOrHibernate}}
          when ?is_timeout(TimeoutOrHibernate);
               TimeoutOrHibernate =:= hibernate ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(
              Parent, Name, State, CbCache, TimeoutOrHibernate,
              HibernateAfterTimeout, Debug);
	{ok, {ok, State, {continue, _}=Continue}} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(
              Parent, Name, State, CbCache, Continue,
              HibernateAfterTimeout, Debug);
	{ok, {stop, Reason}} ->
	    %% For consistency, we must make sure that the
	    %% registered name (if any) is unregistered before
	    %% the parent process is notified about the failure.
	    %% (Otherwise, the parent process could get
	    %% an 'already_started' error if it immediately
	    %% tried starting the process again.)
	    gen:unregister_name(Name0),
            exit(Reason);
	{ok, {error, _Reason} = ERROR} ->
            %% The point of this clause is that we shall have a silent/graceful
            %% termination. The error reason will be returned to the
            %% 'Starter' ({error, Reason}), but *no* crash report.
	    gen:unregister_name(Name0),
	    proc_lib:init_fail(Starter, ERROR, {exit, normal});
	{ok, ignore} ->
	    gen:unregister_name(Name0),
            proc_lib:init_fail(Starter, ignore, {exit, normal});
	{ok, Else} ->
	    gen:unregister_name(Name0),
            exit({bad_return_value, Else});
	{'EXIT', Class, Reason, Stacktrace} ->
	    gen:unregister_name(Name0),
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

loop(Parent, Name, State, CbCache, {continue, Continue} = Msg, HibernateAfterTimeout, Debug) ->
    Reply = try_handle_continue(CbCache, Continue, State),
    case Debug of
        [] ->
            handle_common_reply(Reply, Parent, Name, undefined, Msg, CbCache,
                                HibernateAfterTimeout, State);
        _ ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, Msg),
            handle_common_reply(Reply, Parent, Name, undefined, Msg, CbCache,
                                HibernateAfterTimeout, State, Debug1)
    end;

loop(Parent, Name, State, CbCache, hibernate, HibernateAfterTimeout, Debug) ->
    Mod = CbCache#callback_cache.module,
    proc_lib:hibernate(?MODULE,wake_hib,[Parent, Name, State, Mod, HibernateAfterTimeout, Debug]);

loop(Parent, Name, State, CbCache, infinity, HibernateAfterTimeout, Debug) ->
    receive
        Msg ->
            decode_msg(Msg, Parent, Name, State, CbCache, infinity, HibernateAfterTimeout, Debug, false)
    after HibernateAfterTimeout ->
            loop(Parent, Name, State, CbCache, hibernate, HibernateAfterTimeout, Debug)
    end;

loop(Parent, Name, State, CbCache, Time, HibernateAfterTimeout, Debug) ->
    Msg = receive
              Input ->
                  Input
          after Time ->
                  timeout
          end,
    decode_msg(Msg, Parent, Name, State, CbCache, Time, HibernateAfterTimeout, Debug, false).

-spec create_callback_cache(module()) -> #callback_cache{}.
create_callback_cache(Mod) ->
    #callback_cache{module = Mod,
                    handle_call = fun Mod:handle_call/3,
                    handle_cast = fun Mod:handle_cast/2,
                    handle_info = fun Mod:handle_info/2}.

wake_hib(Parent, Name, State, Mod, HibernateAfterTimeout, Debug) ->
    Msg = receive
              Input ->
                  Input
          end,
    CbCache = create_callback_cache(Mod),
    decode_msg(Msg, Parent, Name, State, CbCache, hibernate, HibernateAfterTimeout, Debug, true).

decode_msg(Msg, Parent, Name, State, CbCache, Time, HibernateAfterTimeout, Debug, Hib) ->
    case Msg of
        {system, From, Req} ->
            sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                                  [Name, State, CbCache, Time, HibernateAfterTimeout], Hib);
        {'EXIT', Parent, Reason} ->
            #callback_cache{module = Mod} = CbCache,
            terminate(Reason, ?STACKTRACE(), Name, undefined, Msg, Mod, State, Debug);
        _Msg when Debug =:= [] ->
            handle_msg(Msg, Parent, Name, State, CbCache, HibernateAfterTimeout);
        _Msg ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3,
                                      Name, {in, Msg}),
            handle_msg(Msg, Parent, Name, State, CbCache, HibernateAfterTimeout, Debug1)
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

%% ---------------------------------------------------
%% Helper functions for try-catch of callbacks.
%% Returns the return value of the callback, or
%% {'EXIT', Class, Reason, Stack} (if an exception occurs)
%%
%% The Class, Reason and Stack are given to erlang:raise/3
%% to make sure proc_lib receives the proper reasons and
%% stacktraces.
%% ---------------------------------------------------

try_dispatch({'$gen_cast', Msg}, CbCache, State) ->
    try_handle_cast(CbCache, Msg, State);
try_dispatch(Info, CbCache, State) ->
    try_handle_info(CbCache, Info, State).

try_handle_continue(#callback_cache{module = Mod}, Msg, State) ->
    try
        {ok, Mod:handle_continue(Msg, State)}
    catch
        throw:R ->
            {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

try_handle_info(#callback_cache{module = Mod, handle_info = HandleInfo}, Msg, State) ->
    try
        {ok, HandleInfo(Msg, State)}
    catch
        throw:R ->
            {ok, R};
        error:undef = R:Stacktrace ->
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

try_handle_cast(#callback_cache{handle_cast = HandleCast}, Msg, State) ->
    try
        {ok, HandleCast(Msg, State)}
    catch
        throw:R ->
            {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

try_handle_call(#callback_cache{handle_call = HandleCall}, Msg, From, State) ->
    try
        {ok, HandleCall(Msg, From, State)}
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

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, CbCache, HibernateAfterTimeout) ->
    Result = try_handle_call(CbCache, Msg, From, State),
    case Result of
	{ok, {reply, Reply, NState}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, CbCache, infinity, HibernateAfterTimeout, []);
	{ok, {reply, Reply, NState, TimeoutOrHibernate}}
          when ?is_timeout(TimeoutOrHibernate);
               TimeoutOrHibernate =:= hibernate ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, CbCache, TimeoutOrHibernate, HibernateAfterTimeout, []);
	{ok, {reply, Reply, NState, {continue, _}=Continue}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, CbCache, Continue, HibernateAfterTimeout, []);
	{ok, {stop, Reason, Reply, NState}} ->
	    try
            Mod = CbCache#callback_cache.module,
		terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, [])
	    after
		reply(From, Reply)
	    end;
	Other -> handle_common_reply(Other, Parent, Name, From, Msg, CbCache, HibernateAfterTimeout, State)
    end;
handle_msg(Msg, Parent, Name, State, CbCache, HibernateAfterTimeout) ->
    Reply = try_dispatch(Msg, CbCache, State),
    handle_common_reply(Reply, Parent, Name, undefined, Msg, CbCache, HibernateAfterTimeout, State).

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, CbCache, HibernateAfterTimeout, Debug) ->
    Result = try_handle_call(CbCache, Msg, From, State),
    case Result of
	{ok, {reply, Reply, NState}} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, CbCache, infinity, HibernateAfterTimeout, Debug1);
	{ok, {reply, Reply, NState, TimeoutOrHibernate}}
          when ?is_timeout(TimeoutOrHibernate);
               TimeoutOrHibernate =:= hibernate ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, CbCache, TimeoutOrHibernate, HibernateAfterTimeout, Debug1);
	{ok, {reply, Reply, NState, {continue, _}=Continue}} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, CbCache, Continue, HibernateAfterTimeout, Debug1);
	{ok, {stop, Reason, Reply, NState}} ->
	    try
            Mod = CbCache#callback_cache.module,
		terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, Debug)
	    after
		_ = reply(Name, From, Reply, NState, Debug)
	    end;
	Other ->
	    handle_common_reply(Other, Parent, Name, From, Msg, CbCache, HibernateAfterTimeout, State, Debug)
    end;
handle_msg(Msg, Parent, Name, State, CbCache, HibernateAfterTimeout, Debug) ->
    Reply = try_dispatch(Msg, CbCache, State),
    handle_common_reply(Reply, Parent, Name, undefined, Msg, CbCache, HibernateAfterTimeout, State, Debug).

handle_common_reply(Reply, Parent, Name, From, Msg, CbCache, HibernateAfterTimeout, State) ->
    Mod = CbCache#callback_cache.module,
    case Reply of
	{ok, {noreply, NState}} ->
	    loop(Parent, Name, NState, CbCache, infinity, HibernateAfterTimeout, []);
	{ok, {noreply, NState, TimeoutOrHibernate}}
          when ?is_timeout(TimeoutOrHibernate);
               TimeoutOrHibernate =:= hibernate ->
	    loop(Parent, Name, NState, CbCache, TimeoutOrHibernate, HibernateAfterTimeout, []);
	{ok, {noreply, NState, {continue, _}=Continue}} ->
	    loop(Parent, Name, NState, CbCache, Continue, HibernateAfterTimeout, []);
	{ok, {stop, Reason, NState}} ->
	    terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, []);
	{'EXIT', Class, Reason, Stacktrace} ->
	    terminate(Class, Reason, Stacktrace, Name, From, Msg, Mod, State, []);
	{ok, BadReply} ->
	    terminate({bad_return_value, BadReply}, ?STACKTRACE(), Name, From, Msg, Mod, State, [])
    end.

handle_common_reply(Reply, Parent, Name, From, Msg, CbCache, HibernateAfterTimeout, State, Debug) ->
    Mod = CbCache#callback_cache.module,
    case Reply of
	{ok, {noreply, NState}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, CbCache, infinity, HibernateAfterTimeout, Debug1);
	{ok, {noreply, NState, TimeoutOrHibernate}}
          when ?is_timeout(TimeoutOrHibernate);
               TimeoutOrHibernate =:= hibernate ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, {noreply, NState}),
	    loop(Parent, Name, NState, CbCache, TimeoutOrHibernate, HibernateAfterTimeout, Debug1);
	{ok, {noreply, NState, {continue, _}=Continue}} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, {noreply, NState}),
	    loop(Parent, Name, NState, CbCache, Continue, HibernateAfterTimeout, Debug1);
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
system_continue(Parent, Debug, [Name, State, CbCache, Time, HibernateAfterTimeout]) ->
    loop(Parent, Name, State, CbCache, Time, HibernateAfterTimeout, Debug).

-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [Name, State, CbCache, _Time, _HibernateAfterTimeout]) ->
    Mod = CbCache#callback_cache.module,
    terminate(Reason, ?STACKTRACE(), Name, undefined, [], Mod, State, Debug).

system_code_change([Name, State, CbCache, Time, HibernateAfterTimeout], _Module, OldVsn, Extra) ->
    Mod = CbCache#callback_cache.module,
    case catch Mod:code_change(OldVsn, State, Extra) of
        {ok, NewState} -> {ok, [Name, NewState, CbCache, Time, HibernateAfterTimeout]};
        Else -> Else
    end.

system_get_state([_Name, State, _Mod, _Time, _HibernateAfterTimeout]) ->
    {ok, State}.

system_replace_state(StateFun, [Name, State, CbCache, Time, HibernateAfterTimeout]) ->
    NState = StateFun(State),
    {ok, NState, [Name, NState, CbCache, Time, HibernateAfterTimeout]}.

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
    Reply = try_terminate(Mod, catch_result(Class, Reason, Stacktrace), State),
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

%% What an old style `catch` would return
catch_result(error, Reason, Stacktrace) -> {Reason, Stacktrace};
catch_result(exit, Reason, _Stacktrace) -> Reason.

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
    [PDict, SysState, Parent, Debug, [Name, State, CbCache, _Time, _HibernateAfterTimeout]] = StatusData,
    Mod = CbCache#callback_cache.module,
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
