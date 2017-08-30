%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_iiop_outproxy.erl
%% 
%% Description:
%%    This file contains the IIOP "proxy" for outgoing connections
%%
%%
%%-----------------------------------------------------------------
-module(orber_iiop_outproxy).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1, request/5, cancel/2, cancel/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/2, stop/1, checkheaders/1]).

%%-----------------------------------------------------------------
%% Macros/Defines
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).

-record(state, {stype, socket, db, timeout, client_timeout, host, port, parent,
		error_reason = {'EXCEPTION', #'COMM_FAILURE'
				{completion_status=?COMPLETED_MAYBE}}}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    ignore.

start(Opts) ->
    gen_server:start_link(orber_iiop_outproxy, Opts, []).

request(Pid, true, Timeout, Msg, RequestId) ->
    %% Why not simply use gen_server:call? We must be able to receive
    %% more than one reply (i.e. fragmented messages).
    MRef = erlang:monitor(process, Pid),
    gen_server:cast(Pid, {request, Timeout, Msg, RequestId, self(), MRef}),
    receive
	{MRef, Reply} ->
	    erlang:demonitor(MRef, [flush]),
            Reply;
	{'DOWN', MRef, _, Pid, _Reason} when is_pid(Pid) ->
            receive
		%% Clear EXIT message from queue
                {'EXIT', _Pid, _What} -> 
                    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE})
            after 0 ->
                    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE})
            end;
	{fragmented, GIOPHdr, Bytes, RequestId, MRef} ->
	    collect_fragments(GIOPHdr, [], Bytes, Pid, RequestId, MRef)
    end;
request(Pid, _, _, Msg, _RequestId) ->
    %% No response expected
    gen_server:cast(Pid, {oneway_request, Msg}).

cancel(Pid, RequestId) ->
    gen_server:cast(Pid, {cancel, RequestId}).

cancel(Pid, RequestId, MRef) ->
    gen_server:cast(Pid, {cancel, RequestId, MRef, self()}).

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/2
%%-----------------------------------------------------------------
stop(Pid, Timeout) ->
    gen_server:call(Pid, stop, Timeout).
stop(Pid) ->
    gen_server:cast(Pid, stop).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({connect, Host, Port, SocketType, SocketOptions, Parent, Key, NewKey}) ->
    process_flag(trap_exit, true), 
    case catch orber_socket:connect(SocketType, Host, Port, 
				    orber_socket:get_ip_family_opts(Host) ++ SocketOptions) of
	{'EXCEPTION', _E} ->
	    ignore;
	%% We used to reply the below but since this would generate a CRASH REPORT
	%% if '-boot start_sasl' used. Due to a request to change this behaviour
	%% we did.
	%% {stop, {'EXCEPTION', E}};
	Socket ->
	    SockData = orber_socket:sockdata(SocketType, Socket),
	    orber_iiop_pm:add_connection(Key, NewKey, SockData),
	    Timeout = orber:iiop_connection_timeout(),
	    {ok, #state{stype = SocketType, socket = Socket,
			db = ets:new(orber_outgoing_requests, [set]),
			timeout = Timeout, client_timeout = orber:iiop_timeout(),
			host = Host, port = Port, parent = Parent}, Timeout}
    end.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(_Reason, #state{db = OutRequests, error_reason = ER}) ->
    %% Kill all proxies and delete table before terminating
    notify_clients(OutRequests, ets:first(OutRequests), ER),
    ets:delete(OutRequests),
    ok.

notify_clients(_, '$end_of_table', _ER) ->
    ok;
notify_clients(OutRequests, Key, ER) ->
    case ets:lookup(OutRequests, Key) of
	[{_, Pid, TRef, MRef}] ->
	    cancel_timer(TRef),
	    Pid ! {MRef, ER},
	    notify_clients(OutRequests, ets:next(OutRequests, Key), ER)
    end.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(X, From, State) ->
    orber:dbg("[~p] orber_iiop_outproxy:handle_call(~p);~n"
	      "Un-recognized call from ~p", [?LINE, X, From], ?DEBUG_LEVEL),
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast({request, Timeout, Msg, RequestId, From, MRef}, 
	    #state{client_timeout = DefaultTimeout} = State) ->
    orber_socket:write(State#state.stype, State#state.socket, Msg),
    true = ets:insert(State#state.db, {RequestId, From, 
				       start_timer(Timeout, DefaultTimeout, RequestId),
				       MRef}),
    {noreply, State, State#state.timeout};
handle_cast({oneway_request, Msg}, State) ->
    orber_socket:write(State#state.stype, State#state.socket, Msg),
    {noreply, State, State#state.timeout};
handle_cast({cancel, ReqId}, State) ->
    case ets:lookup(State#state.db, ReqId) of
	[{ReqId, _From, TRef, _MRef}] ->
	    cancel_timer(TRef),
	    ets:delete(State#state.db, ReqId),
	    orber:dbg("[~p] orber_iiop_outproxy:handle_info(~p);~n"
		      "Request cancelled", [?LINE, State], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout};
	_ ->
	    {noreply, State, State#state.timeout}
    end;
handle_cast({cancel, ReqId, MRef, From}, State) ->
    case ets:lookup(State#state.db, ReqId) of
	[{ReqId, From, TRef, MRef}] ->
	    cancel_timer(TRef),
	    ets:delete(State#state.db, ReqId),
	    From ! {MRef, ReqId, cancelled},
	    orber:dbg("[~p] orber_iiop_outproxy:handle_info(~p); 
Request cancelled", [?LINE, State], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout};
	_ ->
	    From ! {MRef, ReqId, cancelled},
	    {noreply, State, State#state.timeout}
    end;
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(X, State) ->
    orber:dbg("[~p] orber_iiop_outproxy:handle_cast(~p); 
Un-recognized cast.", [?LINE, X], ?DEBUG_LEVEL),
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
handle_info({tcp, _Socket, Bytes}, State) ->
    handle_reply(Bytes, State);
handle_info({ssl, _Socket, Bytes}, State) ->
    handle_reply(Bytes, State);
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State}; 
handle_info({ssl_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, #state{socket = Socket, host = Host,
						port = Port} = State) ->
    orber:error("[~p] IIOP proxy received the TCP error message: ~p~n"
		"The server-side ORB is located at '~p:~p'~n"
		"See the gen_tcp/inet documentation for more information.", 
		[?LINE, Reason, Host, Port], ?DEBUG_LEVEL),
    {stop, normal, State};
handle_info({ssl_error, Socket, Reason}, #state{socket = Socket, host = Host,
						port = Port} = State) ->
    orber:error("[~p] IIOP proxy received the SSL error message: ~p~n"
		"The server-side ORB is located at '~p:~p'~n"
		"See the SSL-application documentation for more information.", 
		[?LINE, Reason, Host, Port], ?DEBUG_LEVEL),
    {stop, normal, State};
handle_info({timeout, _TRef, ReqId}, State) ->
    case ets:lookup(State#state.db, ReqId) of
	[{ReqId, Pid, _, MRef}] ->
	    ets:delete(State#state.db, ReqId),
	    Pid ! {MRef, {'EXCEPTION', #'TIMEOUT'{completion_status=?COMPLETED_MAYBE}}},
	    orber:dbg("[~p] orber_iiop_outproxy:handle_info(~p, ~p);~n"
		      "Request timed out", 
		      [?LINE, State#state.host, State#state.port], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout};
	_ ->
	    {noreply, State, State#state.timeout}
    end;
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(timeout, State) ->
    case ets:info(State#state.db, size) of
	0 ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_info(~p, ~p);~n"
		      "Outgoing connection timed out after ~p msec", 
		      [?LINE, State#state.host, State#state.port,
		       State#state.timeout], ?DEBUG_LEVEL),
	    {stop, normal, State};
	_Amount ->
	    %% Still pending request, cannot close the connection.
	    {noreply, State, State#state.timeout}
    end;
handle_info({'EXIT', Parent, Reason}, #state{parent = Parent} = State) ->
    orber:dbg("[~p] orber_iiop_outproxy:handle_info(~p);~nParent terminated.", 
	      [?LINE, Reason], ?DEBUG_LEVEL),
    {stop, normal, State};
handle_info({reconfigure, _Options}, State) ->
    %% Currently there are no parameters that can be changed.
    {noreply, State, State#state.timeout};
handle_info(X, State) ->
    orber:dbg("[~p] orber_iiop_outproxy:handle_info(~p);~nUn-recognized info.", 
	      [?LINE, X], ?DEBUG_LEVEL),
    {noreply, State, State#state.timeout}.


handle_reply(Bytes, State) ->
    %% Check IIOP headers and fetch request id
    case catch checkheaders(cdr_decode:dec_giop_message_header(Bytes)) of
	{'reply', ReplyHeader, Rest, Len, ByteOrder} ->
	    case ets:lookup(State#state.db, ReplyHeader#reply_header.request_id) of
		[{_, Pid, TRef, MRef}] ->
		    %% Send reply to the correct request process
		    cancel_timer(TRef),
		    Pid ! {MRef, {reply, ReplyHeader, Rest, Len, ByteOrder, Bytes}},
		    ets:delete(State#state.db, ReplyHeader#reply_header.request_id),
		    {noreply, State, State#state.timeout};
		_ ->
		    {noreply, State, State#state.timeout}
	    end;
	{'locate_reply', LocateReplyHeader, LocateRest, LocateLen, LocateByteOrder} ->
	    case ets:lookup(State#state.db, 
			    LocateReplyHeader#locate_reply_header.request_id) of
		[{_, Pid, TRef, MRef}] ->
		    %% Send reply to the correct request process
		    cancel_timer(TRef),
		    Pid ! {MRef, {locate_reply, LocateReplyHeader, 
				  LocateRest, LocateLen, LocateByteOrder}},
		    ets:delete(State#state.db, 
			       LocateReplyHeader#locate_reply_header.request_id),
		    {noreply, State, State#state.timeout};
		_ ->
		    {noreply, State, State#state.timeout}
	    end;
	{fragment, GIOPHdr, ReqId, false} ->
	    %% Last fragment, cancel timer and remove from DB.
	    case ets:lookup(State#state.db, ReqId) of
		[{_, Pid, TRef, MRef}] ->
		    cancel_timer(TRef),
		    Pid ! {fragment, GIOPHdr, ReqId, MRef}, 
		    ets:delete(State#state.db, ReqId),
		    {noreply, State, State#state.timeout};
		_ ->
		    %% Probably cancelled
		    {noreply, State, State#state.timeout}
	    end;
	{fragment, GIOPHdr, ReqId, _} ->
	    %% More fragments expected
	    case ets:lookup(State#state.db, ReqId) of
		[{_, Pid, _, MRef}] ->
		    Pid ! {fragment, GIOPHdr, ReqId, MRef}, 
		    {noreply, State, State#state.timeout};
		_ ->
		    %% Probably cancelled
		    {noreply, State, State#state.timeout}
	    end;
	{fragmented, GIOPHdr, ReqId} ->
	    %% This the initial message (i.e. a LocateReply or Reply).
	    case ets:lookup(State#state.db, ReqId) of
		[{_, Pid, _TRef, MRef}] ->
		    Pid ! {fragmented, GIOPHdr, Bytes, ReqId, MRef},
		    {noreply, State, State#state.timeout};
		_ ->
		    {noreply, State, State#state.timeout}
	    end;
	{'EXCEPTION', DecodeException} ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); decode exception(~p).", 
		      [?LINE, Bytes, DecodeException], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout};
	{'EXIT', message_error} ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); message error.", 
		      [?LINE, Bytes], ?DEBUG_LEVEL),
	    ME = cdr_encode:enc_message_error(#giop_env{version = 
							orber:giop_version()}),
	    orber_socket:write(State#state.stype, State#state.socket, ME),
	    {noreply, State, State#state.timeout};
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); got exit(~p)", 
		      [?LINE, Bytes, R], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout};
	close_connection ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply();
The Server-side ORB closed the connection.", [?LINE], ?DEBUG_LEVEL),
	    {stop, normal, State};
	{error, no_reply} ->
	    {noreply, State, State#state.timeout};
	X ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); message error(~p).", 
		      [?LINE, Bytes, X], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout}
    end.


%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
checkheaders(#giop_message{message_type = ?GIOP_MSG_CLOSE_CONNECTION}) ->
    close_connection;
checkheaders(#giop_message{message_type = ?GIOP_MSG_FRAGMENT,
			   giop_version = {1,2},
			   fragments = MoreFrag} = GIOPHdr) ->
    %% A fragment; we must have received a Request or LocateRequest
    %% with fragment-flag set to true.
    %% We need to decode the header to get the request-id.
    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
				       GIOPHdr#giop_message.message),
    {fragment, GIOPHdr, ReqId, MoreFrag};
checkheaders(#giop_message{fragments = true,
			   giop_version = {1,2}} = GIOPHdr) ->
    %% Must be a Reply or LocateReply which have been fragmented.
    %% We need to decode the header to get the request-id.
    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
				       GIOPHdr#giop_message.message),
    {fragmented, GIOPHdr, ReqId};
checkheaders(#giop_message{fragments = false, 
			   message_type = ?GIOP_MSG_REPLY} = GIOPHdr) ->
    {ReplyHeader, Rest, Len} = 
	cdr_decode:dec_reply_header(GIOPHdr#giop_message.giop_version,
				    GIOPHdr#giop_message.message, 
				    ?GIOP_HEADER_SIZE,
				    GIOPHdr#giop_message.byte_order),
    {'reply', ReplyHeader, Rest, Len, GIOPHdr#giop_message.byte_order};
checkheaders(#giop_message{fragments = false, 
			   message_type = ?GIOP_MSG_LOCATE_REPLY} = GIOPHdr) ->
    {LocateReplyHeader, Rest, Len} = 
	cdr_decode:dec_locate_reply_header(GIOPHdr#giop_message.giop_version,
					   GIOPHdr#giop_message.message, 
					   ?GIOP_HEADER_SIZE,
					   GIOPHdr#giop_message.byte_order),
    {'locate_reply', LocateReplyHeader, Rest, Len, GIOPHdr#giop_message.byte_order};
checkheaders(What) ->
    orber:dbg("[~p] orber_iiop_outproxy:checkheaders(~p)
Un-recognized GIOP header.", [?LINE, What], ?DEBUG_LEVEL),
    {error, no_reply}.


cancel_timer(infinity) -> 
    ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

start_timer(infinity, infinity, _) -> 
    infinity;
start_timer(infinity, Timeout, RequestId) ->
    erlang:start_timer(Timeout, self(), RequestId);
start_timer(Timeout, _, RequestId) ->
    erlang:start_timer(Timeout, self(), RequestId).



collect_fragments(GIOPHdr1, InBuffer, Bytes, Proxy, RequestId, MRef) ->
    receive
	%% There are more framents to come; just collect this message and wait for
	%% the rest.
	{fragment, #giop_message{byte_order = _ByteOrder,
				 message    = Message,
				 fragments  = true} = GIOPHdr2, RequestId, MRef} ->
	    case catch cdr_decode:dec_message_header(null, GIOPHdr2, Message) of
		{_, #fragment_header{}, FragBody, _, _} ->
		    collect_fragments(GIOPHdr1, [FragBody|InBuffer], 
				      Bytes, Proxy, RequestId, MRef);
		Other ->
		    cancel(Proxy, RequestId, MRef),
		    clear_queue(Proxy, RequestId, MRef),
		    orber:dbg("[~p] orber_iiop:collect_fragments(~p)", 
			      [?LINE, Other], ?DEBUG_LEVEL),
		    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 18), 
					   completion_status=?COMPLETED_YES})
	    end;
	%% This is the last fragment. Now we can but together the fragments, decode
	%% the reply and send it to the client.
	{fragment, #giop_message{byte_order = ByteOrder,
				 message    = Message} = GIOPHdr2, RequestId, MRef} ->
	    erlang:demonitor(MRef, [flush]),
	    case catch cdr_decode:dec_message_header(null, GIOPHdr2, Message) of
		{_, #fragment_header{}, FragBody, _, _} ->
		    %% This buffer is all the fragments concatenated.
		    Buffer = lists:reverse([FragBody|InBuffer]),
		    
		    %% Create a GIOP-message which is exactly as if hadn't been fragmented.
		    NewGIOP = GIOPHdr1#giop_message
				{message = list_to_binary([GIOPHdr1#giop_message.message|Buffer]),
				 fragments = false},
		    case checkheaders(NewGIOP) of
			{'reply', ReplyHeader, Rest, Len, ByteOrder} ->
			    %% We must keep create a copy of all bytes, as if the 
			    %% message wasn't fragmented, to be able handle TypeCode
			    %% indirection.
			    {'reply', ReplyHeader, Rest, Len, ByteOrder, 
			     list_to_binary([Bytes|Buffer])};
			{'locate_reply', ReplyHdr, Rest, Len, ByteOrder} ->
			    {'locate_reply', ReplyHdr, Rest, Len, ByteOrder};
			Error ->
			    orber:dbg("[~p] orber_iiop:collect_fragments(~p, ~p);
Unable to decode Reply or LocateReply header",[?LINE, NewGIOP, Error], ?DEBUG_LEVEL),
			    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 18),
						   completion_status=?COMPLETED_YES})
		    end;
		Other ->
		    orber:dbg("[~p] orber_iiop:collect_fragments(~p);", 
			      [?LINE, Other], ?DEBUG_LEVEL),
		    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 18),
					   completion_status=?COMPLETED_YES})
	    end;
	{MRef, {'EXCEPTION', E}} ->
	    orber:dbg("[~p] orber_iiop:collect_fragments(~p);", 
		      [?LINE, E], ?DEBUG_LEVEL),
	    erlang:demonitor(MRef, [flush]),
            corba:raise(E);
	{'DOWN', MRef, _, Proxy, Reason} when is_pid(Proxy) ->
	    orber:dbg("[~p] orber_iiop:collect_fragments(~p);~n"
		      "Monitor generated a DOWN message.", 
		      [?LINE, Reason], ?DEBUG_LEVEL),
            receive
		%% Clear EXIT message from queue
                {'EXIT', _Proxy, _What} -> 
                    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE})
            after 0 ->
                    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE})
            end
    end.

clear_queue(Proxy, RequestId, MRef) ->
    receive 
	{fragment, _, RequestId, MRef} ->
	    clear_queue(Proxy, RequestId, MRef);
	{MRef, RequestId, cancelled} ->
	    %% This is the last message that the proxy will send
	    %% after we've cancelled the request.
	    erlang:demonitor(MRef, [flush]),
	    ok;
	{'DOWN', MRef, _, Proxy, _Reason} ->
	    %% The proxy terminated. Clear EXIT message from queue
            receive
                {'EXIT', Proxy, _What} -> 
                    ok
            after 0 ->
                    ok
            end
    end.
	    
