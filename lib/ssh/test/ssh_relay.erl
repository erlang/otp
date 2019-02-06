%%%-------------------------------------------------------------------
%%% @author Simon Cornish <simon@cali.coffee>
%%% @copyright (C) 2015, Simon Cornish
%%% @doc
%%% Provide manipulatable TCP-level relaying for testing SSH
%%% @end
%%% Created :  7 May 2015 by Simon Cornish <simon@cali.coffee>
%%%-------------------------------------------------------------------
-module(ssh_relay).

-behaviour(gen_server).

%% API
-export([start_link/4]).
-export([stop/1]).
-export([hold/4, release/2, release_next/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(hold, {
	  port,
	  n,
	  tmo,
	  tref,
	  q = []
	 }).

-record(state, {
	  local_addr,
	  local_port,
	  peer_addr,
	  peer_port,
	  lpid,
	  local,
	  peer,
	  tx_hold,
	  rx_hold
	 }).

-define(ACCEPT_TMO, 200).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Hold N (or 'all') messages in given direction.
%% Messages will be released after the N+1th message or
%% Tmo ms or 'infinity'
%%
%% Dir is 'tx' for direction local -> peer
%%    and 'rx' for direction peer  -> local
%%
%% An Error, ealready, is returned if there is already a hold
%% in the given direction
%%
%% @spec hold(Srv, Dir, N, Tmo) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
hold(Srv, Dir, N, Tmo) ->
    gen_server:call(Srv, {hold, Dir, N, Tmo}).

%%--------------------------------------------------------------------
%% @doc
%% Release all held messages in given direction.
%%
%% An Error, enoent, is returned if there is no hold
%% in the given direction
%%
%% @spec release(Srv, Dir) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
release(Srv, Dir) ->
    gen_server:call(Srv, {release, Dir}).

%%--------------------------------------------------------------------
%% @doc
%% Release all held messages in given direction after the 
%% next message in the trigger direction
%%
%% An Error, enoent, is returned if there is no hold
%% in the given direction
%%
%% @spec release_next(Srv, Dir, TriggerDir) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
release_next(Srv, Dir, TriggerDir) ->
    gen_server:call(Srv, {release_next, Dir, TriggerDir}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ListenAddr, ListenPort, PeerAddr, PeerPort) ->
    gen_server:start_link(?MODULE, [ListenAddr, ListenPort, PeerAddr, PeerPort], []).

stop(Srv) ->
    unlink(Srv),
    Srv ! stop.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ListenAddr, ListenPort, PeerAddr, PeerPort | _Options]) ->
    IfAddr = case ListenAddr of
		 {0,0,0,0} ->
		     [];
		 _ ->
		     [{ifaddr, ListenAddr}]
	     end,
    case gen_tcp:listen(ListenPort, [{reuseaddr, true}, {backlog, 1}, {active, false}, binary | IfAddr]) of
	{ok, LSock} ->
	    Parent = self(),
	    {LPid, _LMod} = spawn_monitor(fun() -> listen(Parent, LSock) end),
	    S = #state{local_addr = ListenAddr,
		       local_port = ListenPort,
		       lpid = LPid,
		       peer_addr = ssh_test_lib:ntoa(
                                     ssh_test_lib:mangle_connect_address(PeerAddr)),
		       peer_port = PeerPort
		      },
	    {ok, S};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({hold, Dir, N, Tmo}, _From, State) ->
    case Dir of
	tx ->
	    do_hold(#state.tx_hold, State#state.peer, N, Tmo, State);
	rx ->
	    do_hold(#state.rx_hold, State#state.local, N, Tmo, State);
	_ ->
	    {reply, {error, einval}, State}
    end;
handle_call({release, Dir}, _From, State) ->
    case Dir of
	tx ->
	    do_release(#state.tx_hold, State);
	rx ->
	    do_release(#state.rx_hold, State);
	_ ->
	    {reply, {error, einval}, State}
    end;
handle_call({release_next, _Dir, _TriggerDir}, _From, State) ->
    {reply, {error, nyi}, State};

handle_call(Request, _From, State) ->
    Reply = {unhandled, Request},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Local, Data}, S) when S#state.local == Local ->
    S1 = do_local(Data, S),
    {noreply, S1};

handle_info({tcp_error, Local, Error}, S) when S#state.local == Local ->
    S1 = do_local({error, Error}, S),
    {noreply, S1};

handle_info({tcp_closed, Local}, S) when S#state.local == Local ->
    S1 = do_local(closed, S),
    {noreply, S1};

handle_info({tcp, Peer, Data}, S) when S#state.peer == Peer ->
    S1 = do_peer(Data, S),
    {noreply, S1};

handle_info({tcp_error, Peer, Error}, S) when S#state.peer == Peer ->
    S1 = do_peer({error, Error}, S),
    {noreply, S1};

handle_info({tcp_closed, Peer}, S) when S#state.peer == Peer ->
    S1 = do_peer(closed, S),
    {noreply, S1};

handle_info({accept, Local}, S) ->
    S1 = do_accept(Local, S),
    {noreply, S1};

handle_info({activate, Local}, State) ->
    inet:setopts(Local, [{active, true}]),
    {noreply, State};

handle_info({release, Pos}, S) ->
    {reply, _, S1} = do_release(Pos,S),
    {noreply, S1};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info({'DOWN', _Ref, _process, LPid, Reason}, S) when S#state.lpid == LPid ->
    io:format("Acceptor in ~p has finished: ~p~n", [?MODULE,Reason]),
    {noreply, S};

handle_info(_Info, State) ->
    io:format("~p:~p Unhandled info: ~p~n", [?MODULE,?LINE,_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_hold(Pos, _Port, _N, _Tmo, S) when element(Pos, S) /= undefined ->
    {reply, {error, ealready}, S};
do_hold(Pos, Port, N, Tmo, S) ->
    TRef = if is_integer(Tmo) andalso Tmo > 0 ->
		   erlang:send_after(Tmo, self(), {release, Pos});
	      true ->
		   undefined
	   end,
    Hold = #hold{port = Port, n = N, tmo = Tmo, tref = TRef},
    {reply, ok, setelement(Pos, S, Hold)}.

do_release(HPos, S) when element(HPos, S) == undefined ->
    {reply, {error, enoent}, S};
do_release(HPos, S) ->
    #hold{port = Port, tref = TRef, q = Q} = element(HPos, S),
    lists:foreach(fun(M) -> gen_tcp:send(Port, M), erlang:yield() end, Q),
    catch erlang:cancel_timer(TRef),
    receive
	{release, HPos} -> ok
    after 0 ->
	    ok
    end,
    {reply, ok, setelement(HPos, S, undefined)}.

listen(Parent, LSock) ->
    monitor(process, Parent),
    do_listen(Parent, LSock).

do_listen(Parent, LSock) ->
    %% So annoying there is no select-like sematic for this
    case gen_tcp:accept(LSock, ?ACCEPT_TMO) of
	{ok, Sock} ->
	    Parent ! {accept, Sock},
	    gen_tcp:controlling_process(Sock, Parent),
	    Parent ! {activate, Sock},
	    do_flush(Parent, Sock),
	    gen_tcp:close(LSock);
	{error, timeout} ->
	    receive 
		DOWN when element(1, DOWN) == 'DOWN' ->
		    ok;
		stop ->
		    ok
	    after 1 ->
		    do_listen(Parent, LSock)
	    end;
	Error ->
	    gen_tcp:close(LSock),
	    exit({accept,Error})
    end.

do_flush(Parent, Sock) ->
    receive 
	{Tcp, Sock, _} = Msg when Tcp == tcp; Tcp == tcp_error ->
	    Parent ! Msg,
	    do_flush(Parent, Sock);
	{tcp_closed, Sock} = Msg ->
	    Parent ! Msg,
	    do_flush(Parent, Sock)
    after 1 ->
	    ok
    end.

do_accept(Local, S) ->
    case gen_tcp:connect(S#state.peer_addr, S#state.peer_port, [{active, true}, binary]) of
	{ok, Peer} ->
	    S#state{local = Local, peer = Peer};
	Error ->
	    exit({connect, Error})
    end.

do_local(Data, S) when is_binary(Data) ->
    TxH = S#state.tx_hold,
    if TxH == undefined ->
	    gen_tcp:send(S#state.peer, Data),
	    S;
       TxH#hold.n == 0 ->
	    lists:foreach(fun(M) -> gen_tcp:send(S#state.peer, M) end, TxH#hold.q),
	    gen_tcp:send(S#state.peer, Data),
	    catch erlang:cancel_timer(TxH#hold.tref),
	    TxP = #state.tx_hold,
	    receive
		{release, TxP} ->
		    ok
	    after 0 ->
		    ok
	    end,
	    S#state{tx_hold = undefined};
       true ->
	    Q = TxH#hold.q ++ [Data],
	    N = if is_integer(TxH#hold.n) ->
			TxH#hold.n -1;
		   true ->
			TxH#hold.n
		end,
	    S#state{tx_hold = TxH#hold{q = Q, n = N}}
    end;
do_local(Error, _S) ->
    exit({local, Error}).

do_peer(Data, S) when is_binary(Data) ->
    RxH = S#state.rx_hold,
    if RxH == undefined ->
	    gen_tcp:send(S#state.local, Data),
	    S;
       RxH#hold.n == 0 ->
	    lists:foreach(fun(M) -> gen_tcp:send(S#state.local, M) end, RxH#hold.q),
	    gen_tcp:send(S#state.local, Data),
	    catch erlang:cancel_timer(RxH#hold.tref),
	    RxP = #state.rx_hold,
	    receive
		{release, RxP} ->
		    ok
	    after 0 ->
		    ok
	    end,
	    S#state{rx_hold = undefined};
       true ->
	    Q = RxH#hold.q ++ [Data],
	    N = if is_integer(RxH#hold.n) ->
			RxH#hold.n -1;
		   true ->
			RxH#hold.n
		end,
	    S#state{rx_hold = RxH#hold{q = Q, n = N}}
    end;
do_peer(Error, _S) ->
    exit({peer, Error}).

