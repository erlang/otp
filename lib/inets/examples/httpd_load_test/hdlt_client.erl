%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% Purpose: The HDLT client module.
%%          This is the traffic generator
%%----------------------------------------------------------------------

-module(hdlt_client).

-export([
	 start/1, 
	 stop/0, 
	 start_inets/0, 
	 start_service/1, 
	 release/0, 
	 node_info/0
	]).

-export([
	 proxy/1
	]).

-include("hdlt_logger.hrl").

-define(CTRL,   hdlt_ctrl).
-define(PROXY,  hdlt_proxy).

-record(state, 
	{
	 mode = initial,
	 send_rate,
	 time, 
	 stop_time, 
	 url,
	 nof_reqs = 0,
	 nof_reps = 0,
	 last_req,
	 sizes,
	 socket_type, 
	 cert_file
	}).



start(Debug) ->
    proc_lib:start_link(?MODULE, proxy, [Debug]).

stop() ->
    (catch erlang:send(?PROXY, stop)),
    ok.

start_inets() ->
    ?PROXY ! start_inets.

start_service(Args) ->
    ?PROXY ! {start_client, Args, self()},
    receive
	client_started ->
	    %% ?LOG("client service started"),
	    ok
    end.

release() ->
    ?PROXY ! release.

node_info() ->
    ?PROXY ! {node_info, self()},
    receive 
	{node_info, NodeInfo} ->
	    NodeInfo
    end.


%% ---------------------------------------------------------------------
%% 
%% The proxy process
%% 

proxy(Debug) ->
    process_flag(trap_exit, true),
    erlang:register(?PROXY, self()),
    SName = lists:flatten(
	      io_lib:format("HDLT PROXY[~p,~p]", [self(), node()])), 
    ?SET_NAME(SName),
    ?SET_LEVEL(Debug), 
    ?LOG("starting", []),
    Ref = await_for_controller(10), 
    CtrlNode = node(Ref), 
    erlang:monitor_node(CtrlNode, true),
    proc_lib:init_ack({ok, self()}),
    ?DEBUG("started", []),
    proxy_loop(Ref, CtrlNode, undefined).

await_for_controller(N) when N > 0 ->
    case global:whereis_name(hdlt_ctrl) of
	Pid when is_pid(Pid) ->
	    erlang:monitor(process, Pid);
	_ ->
	    timer:sleep(1000),
	    await_for_controller(N-1)
    end;
await_for_controller(_) ->
    proc_lib:init_ack({error, controller_not_found, nodes()}),
    timer:sleep(500),
    init:stop().


proxy_loop(Ref, CtrlNode, Client) ->
    ?DEBUG("await command", []),
    receive
	stop ->
	    ?LOG("stop", []),
	    timer:sleep(1000),
	    halt();

	start_inets ->
	    ?LOG("start the inets service framework", []),
	    %% inets:enable_trace(max, "/tmp/inets-httpc-trace.log", all),
	    case (catch inets:start()) of
		ok ->
		    ?LOG("framework started", []),
		    proxy_loop(Ref, CtrlNode, Client);
		Error ->
		    ?LOG("failed starting inets service framework: "
			"~n   Error: ~p", [Error]),
		    timer:sleep(1000),
		    halt()
	    end;

	{start_client, Args, From} ->
	    ?LOG("start client with"
		"~n   Args: ~p", [Args]),
	    Client2 = spawn_link(fun() -> client(Args) end),
	    From ! client_started,
	    proxy_loop(Ref, CtrlNode, Client2);

	release ->
	    ?LOG("release", []),
	    Client ! go,
	    proxy_loop(Ref, CtrlNode, Client);

	{node_info, Pid} ->
	    ?LOG("received requets for node info", []),
	    NodeInfo = get_node_info(),
	    Pid ! {node_info, NodeInfo}, 
	    proxy_loop(Ref, CtrlNode, Client);

	{'EXIT', Client, normal} ->
	    ?LOG("received normal exit message from client (~p)", 
		 [Client]),
	    exit(normal);
	
	{'EXIT', Client, Reason} ->
	    ?INFO("received exit message from client (~p)"
		 "~n   Reason: ~p", [Client, Reason]),
	    %% Unexpected client termination, inform the controller and die
	    global:send(hdlt_ctrl, {client_exit, Client, node(), Reason}),
	    exit({client_exit, Reason});

	{nodedown, CtrlNode} ->
	    ?LOG("received nodedown for controller node - terminate", []), 
	    halt();

	{'DOWN', Ref, process, _, _} ->
	    ?INFO("received DOWN message for controller - terminate", []),
	    %% The controller has terminated, dont care why, time to die
	    halt()

    end.



%% ---------------------------------------------------------------------
%% 
%% The client process
%% 

client([SocketType, CertFile, URLBase, Sizes, Time, SendRate, Debug]) ->
    SName = lists:flatten(
	      io_lib:format("HDLT CLIENT[~p,~p]", [self(), node()])), 
    ?SET_NAME(SName),
    ?SET_LEVEL(Debug), 
    ?LOG("starting with"
	 "~n   SocketType: ~p"
	 "~n   Time:       ~p"
	 "~n   SendRate:   ~p", [SocketType, Time, SendRate]),
    httpc:set_options([{max_pipeline_length, 0}]),
    if
	(SocketType =:= ssl) orelse
	(SocketType =:= ossl) orelse
	(SocketType =:= essl) ->
	    %% Ensure crypto and ssl started:
	    crypto:start(),
	    ssl:start();
	true ->
	    ok
    end,
    State = #state{mode        = idle, 
		   url         = URLBase, 
		   time        = Time, 
		   send_rate   = SendRate,
		   sizes       = Sizes,
		   socket_type = SocketType,
		   cert_file   = CertFile},
    ?DEBUG("started", []),
    client_loop(State).

%% The point is to first start all client nodes and then this
%% process. Then, when they are all started, the go-ahead, go, 
%% message is sent to let them lose at the same time.
client_loop(#state{mode      = idle, 
		   time      = Time, 
		   send_rate = SendRate} = State) ->
    ?DEBUG("[idle] awaiting the go command", []),
    receive 
	go ->
	    ?LOG("[idle] received go", []),
	    erlang:send_after(Time, self(), stop),
	    NewState = send_requests(State, SendRate),	    
	    client_loop(NewState#state{mode     = generating, 
				       nof_reqs = SendRate})
    end;

%% In this mode the client is generating traffic.
%% It will continue to do so until the stop message
%% is received. 
client_loop(#state{mode = generating} = State) -> 
    receive 
	stop ->
	    ?LOG("[generating] received stop", []),
	    StopTime = timestamp(), 
	    req_reply(State),
	    client_loop(State#state{mode = stopping, stop_time = StopTime});

	{http, {_, {{_, 200, _}, _, _}}} ->
	    %% ?DEBUG("[generating] received reply - send another request", []),
	    NewState = send_requests(State, 1),
	    client_loop(NewState#state{nof_reps = NewState#state.nof_reps + 1,
				       nof_reqs = NewState#state.nof_reqs + 1});

	{http, {ReqId, {error, Reason}}} ->
	    ?INFO("[generating] request ~p failed: "
		  "~n   Reason:  ~p"
		  "~n   NofReqs: ~p"
		  "~n   NofReps: ~p", 
		  [ReqId, Reason, State#state.nof_reqs, State#state.nof_reps]),
	    exit({Reason, generating, State#state.nof_reqs, State#state.nof_reps});

	Else ->
	    ?LOG("[generating] received unexpected message: "
		 "~n~p", [Else]),
	    unexpected_data(Else), 
	    client_loop(State)
    end;

%% The client no longer issues any new requests, instead it 
%% waits for replies for all the oustanding requests to 
%% arrive.
client_loop(#state{mode     = stopping, 
		   time     = Time, 
		   last_req = LastReqId} = State) ->
    receive 
	{http, {LastReqId, {{_, 200, _}, _, _}}} ->
	    ?DEBUG("[stopping] received reply for last request (~p)", [LastReqId]),
	    time_to_complete(State),
	    ok;

	{http, {ReqId, {{_, 200, _}, _, _}}} ->
	    ?DEBUG("[stopping] received reply ~p", [ReqId]),
	    client_loop(State);

	{http, {ReqId, {error, Reason}}} ->
	    ?INFO("[stopping] request ~p failed: "
		  "~n   Reason:  ~p"
		  "~n   NofReqs: ~p"
		  "~n   NofReps: ~p", 
		  [ReqId, Reason, State#state.nof_reqs, State#state.nof_reps]),
	    exit({Reason, stopping, State#state.nof_reqs, State#state.nof_reps});

	Else ->
	    ?LOG("[stopping] received unexpected message: "
		 "~n~p", [Else]),
	    unexpected_data(Else), 
	    client_loop(State)

    after Time ->
	    ?INFO("timeout when"
		  "~n   Number of requests: ~p"
		  "~n   Number of replies:  ~p", 
		  [State#state.nof_reqs, State#state.nof_reps]),
	    exit({timeout, State#state.nof_reqs, State#state.nof_reps})
    end.

req_reply(#state{nof_reqs = NofReqs, nof_reps = NofReps}) ->
    load_data({req_reply, node(), NofReqs, NofReps}).

time_to_complete(#state{stop_time = StopTime}) ->
    StoppedTime = os:timestamp(),
    load_data({time_to_complete, node(), StopTime, StoppedTime}).

load_data(Data) ->
    global:send(?CTRL, {load_data, Data}).

unexpected_data(Else) ->
    global:send(?CTRL, {unexpected_data, Else}).


send_requests(#state{sizes = Sizes} = State, N) ->
    send_requests(State, N, Sizes).

send_requests(State, 0, Sizes) ->
    State#state{sizes = Sizes};
send_requests(#state{socket_type = SocketType, 
		     cert_file   = CertFile} = State, N, [Sz | Sizes]) ->
    URL = lists:flatten(io_lib:format("~s~w", [State#state.url, Sz])),
    Method      = get,
    Request     = {URL, []},
    HTTPOptions = 
	case SocketType of
	    ip_comm ->
		[];
	    _ ->
		SslOpts = [{verify, 0},
			   {certfile, CertFile},
			   {keyfile,  CertFile}],
		case SocketType of
		    ssl ->
			[{ssl, SslOpts}];
		    ossl ->
			[{ssl, {ossl, SslOpts}}];
		    essl ->
			[{ssl, {essl, SslOpts}}]
		end
	end,
    Options = [{sync, false}], 
    {ok, Ref} = httpc:request(Method, Request, HTTPOptions, Options), 
    send_requests(State#state{last_req = Ref}, N-1, lists:append(Sizes, [Sz])).


timestamp() ->
   os:timestamp().
 

get_node_info() ->
    [{cpu_topology,        erlang:system_info(cpu_topology)},
     {heap_type,           erlang:system_info(heap_type)},
     {nof_schedulers,      erlang:system_info(schedulers)},
     {otp_release,         erlang:system_info(otp_release)}, 
     {version,             erlang:system_info(version)}, 
     {system_version,      erlang:system_info(system_version)},
     {system_architecture, erlang:system_info(system_architecture)}].


