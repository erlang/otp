%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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
%% Purpose: Generic megaco transport simulator module
%%----------------------------------------------------------------------

-module(megaco_test_generic_transport).

-behaviour(gen_server).
-behaviour(megaco_transport).

-export([
	 start_transport/0, 
	 listen/2, 
	 connect/2, 
	 start/1,
	 stop/0,
	 incomming_message/2
	]).

%% gen_server callbacks
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2,
	 code_change/3
	]).

%% megaco_transport callbacks
-export([
	 send_message/2,
	 send_message/3,
	 resend_message/2
	]).

-record(state, {parent, 
		controller, 
		receive_handle}).

-include_lib("megaco/include/megaco.hrl").
-include("megaco_test_lib.hrl").
-define(SERVER, ?MODULE).


%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

start(RH) ->
    {ok, Pid} = start_transport(),
    {ok, SendHandle, _} = connect(Pid, [{receive_handle, RH}]),
    {ok, SendHandle}.

start_transport() ->
    %% GS_ARGS = [{debug,[trace]}], 
    GS_ARGS = [], 
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], GS_ARGS),
    unlink(Pid),
    {ok, Pid}.

connect(Sup, Opts) ->
    call({connect, Sup, Opts}).

listen(Sup, Opts) ->
    call({listen, Sup, Opts}).

stop() ->
    call(stop).


%%----------------------------------------------------------------------
%% Megaco transport callback
%%----------------------------------------------------------------------

send_message(SendHandle, Bin) ->
    call({transport, {send_message, SendHandle, Bin}}).

send_message(SendHandle, Bin, Resend) ->
    call({transport, {send_message, SendHandle, Bin, Resend}}).

resend_message(SendHandle, Bin) ->
    call({transport, {resend_message, SendHandle, Bin}}).

incomming_message(Pid, Msg) ->
    cast(Pid, {incomming_message, Msg}).


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Parent]) ->
    {ok, #state{parent = Parent}}.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({connect, _Sup, Opts}, _From, State) ->
    d("handle_call(connect) -> entry with"
      "~n   Opts: ~p", [Opts]),
    {value, {_, ReceiveHandle}} = lists:keysearch(receive_handle, 1, Opts),
    {value, {_, Controller}}    = lists:keysearch(port, 1, Opts),
    SendHandle = self(), 
    ControlPid = self(),
    Reply  = {ok, SendHandle, ControlPid},
    {reply, Reply, State#state{controller     = Controller,
			       receive_handle = ReceiveHandle}};

handle_call({listen, _Sup, Opts}, _From, State) ->
    d("handle_call(listen) -> entry with"
      "~n   Opts: ~p", [Opts]),
    {value, {_, ReceiveHandle}} = lists:keysearch(receive_handle, 1, Opts),
    {value, {_, Controller}}    = lists:keysearch(port, 1, Opts),
    SendHandle = self(), 
    ControlPid = self(),
    Reply  = {ok, SendHandle, ControlPid},
    Controller ! {listen, ReceiveHandle, SendHandle, ControlPid},  
    {reply, Reply, State#state{controller     = Controller,
			       receive_handle = ReceiveHandle}};

handle_call(stop, _From, State) ->
    d("handle_call(stop) -> entry"),
    Reply  = ok,
    Reason = normal, 
    {stop, Reason, Reply, State};

handle_call({transport, Event}, _From, 
	    #state{controller = Pid, receive_handle = RH} = State) ->
    d("handle_call(transport) -> entry with"
      "~n   Event: ~p", [Event]),
    Reply = handle_transport(Pid, RH, Event),
    {reply, Reply, State};

handle_call(Req, From, State) ->
    d("handle_call -> entry with"
      "~n   Req: ~p", [Req]),
    Reply  = {error, {unknown_request, Req}}, 
    Reason = {received_unexpected_request, Req, From},
    {stop, Reason, Reply, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({incomming_message, Msg}, 
	    #state{receive_handle = RH} = State) ->
    d("handle_cast(incomming_message) -> entry with"
      "~n   Msg: ~p", [Msg]),
    handle_incomming_message(Msg, RH),
    {noreply, State};

handle_cast(Msg, State) ->
    d("handle_cast -> entry with"
      "~n   Msg: ~p", [Msg]),
    Reason = {received_unexpected_message, Msg},
    {stop, Reason, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    d("handle_info -> entry with"
      "~n   Info: ~p", [Info]),
    Reason = {received_unexpected_info, Info},
    {stop, Reason, State}.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

handle_transport(Pid, 
		 #megaco_receive_handle{encoding_mod    = EM,
					encoding_config = EC},
		 {Event, SendHandle, Bin, Resend}) ->
    Info = 
	case (catch EM:decode_message(EC, Bin)) of
	    {ok, MegMsg} ->
		{message, MegMsg, Resend};
	    Error ->
		d("handle_transport -> decode failed"
		  "~n   Error: ~p", [Error]),
		{bad_message, Error, Bin}
	end,
    handle_transport(Pid, Event, SendHandle, Info);
handle_transport(Pid, 
		 #megaco_receive_handle{encoding_mod    = EM,
					encoding_config = EC},
		 {Event, SendHandle, Bin}) ->
    Info = 
	case (catch EM:decode_message(EC, Bin)) of
	    {ok, MegMsg} ->
		{message, MegMsg};
	    Error ->
		d("handle_transport -> decode failed"
		  "~n   Error: ~p", [Error]),
		{bad_message, Error, Bin}
	end,
    handle_transport(Pid, Event, SendHandle, Info).

handle_transport(Pid, Event, SendHandle, Info) ->
    Pid ! {transport_event, {Event, SendHandle, Info}, self()},
    receive
	{transport_reply, Reply, Pid} ->
	    d("handle_transport -> received reply"
	      "~n   Reply: ~p", [Reply]),
	    Reply
    after 10000 ->
	    receive
		Any ->
		    d("handle_transport -> received crap after timeout"
		      "~n   Any: ~p", [Any]),
		    exit({timeout, Any})
	    after 0 ->
		    d("handle_transport -> timeout"),
		    exit(timeout)
	    end
    end.


%% This function is used to simulate incomming messages
handle_incomming_message(Msg, 
			 #megaco_receive_handle{encoding_mod    = EM,
						encoding_config = EC} = RH) ->
    Self = self(),
    case EM:encode_message(EC, Msg) of
	{ok, Bin} ->
	    ProcessMessage = 
		fun() -> 
			megaco:process_received_message(RH, Self, Self, Bin) 
		end,
	    spawn(ProcessMessage),
	    ok;
	Error ->
	    d("handle_incomming_message -> encode failed"
	      "~n   Error: ~p", [Error]),
	    exit(Error)
    end.
			  

%%-------------------------------------------------------------------

call(Req) ->
    call(Req, infinity).

call(Req, Timeout) ->
    case (catch gen_server:call(?SERVER, Req, Timeout)) of
        {'EXIT', _} ->
            {error, not_started};
        Res ->
            Res
    end.

%% cast(Msg) ->
%%     cast(whereis(?SERVER), Msg).

cast(Pid, Msg) ->
    d("cast -> entry with"
      "~n   Pid: ~p"
      "~n   Msg: ~p", [Pid, Msg]),
    case (catch gen_server:cast(Pid, Msg)) of
        {'EXIT', Reason} ->
	    d("cast -> failed casting"
	      "~n   Reason: ~p", [Reason]),
            {error, not_started};
        Res ->
            Res
    end.
    

%%-------------------------------------------------------------------

d(F) ->
    d(F, []).

d(F, A) ->
    print(F, A).


print(F, A) ->
    io:format("*** [~s] GENERIC TRANSPORT [~p] ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).

