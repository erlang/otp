%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%
%%--------------------------------------------------------------------
%% File: orber_iiop_inrequest.erl
%% 
%% Description:
%%    This file contains the handling of incomming requests
%%
%%-----------------------------------------------------------------
-module(orber_iiop_inrequest).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/orber_pi.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/5, start_fragment_collector/8]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([handle_message/5, fragment_collector/8]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 8).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(GIOPHdr, Message, Type, Socket, Env) ->
    spawn_link(orber_iiop_inrequest, handle_message, 
	       [GIOPHdr, Message, Type, Socket, Env]).

start_fragment_collector(GIOPHdr, Message, Type, Socket, ReqId, Proxy, MaxFrags, Env) ->
    spawn_link(orber_iiop_inrequest, fragment_collector, 
	       [GIOPHdr, Message, Type, Socket, ReqId, Proxy, MaxFrags, Env]).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: fragment_collector/4
%%-----------------------------------------------------------------
fragment_collector(GIOPHdr, Bytes, SocketType, Socket, ReqId, Proxy, MaxFrags, Env) ->
    case catch collect(Proxy, [], GIOPHdr#giop_message.byte_order, ReqId, 
		       MaxFrags, 0) of
	{ok, Buffer} ->
	    NewGIOP = GIOPHdr#giop_message
			{message = list_to_binary([GIOPHdr#giop_message.message|Buffer])},
	    %% NOTE, the third argument to dec_message_header must be complete
	    %% message (i.e. AllBytes), otherwise we cannot handle indirection.
	    case handle_message(NewGIOP, list_to_binary([Bytes| Buffer]), 
				SocketType, Socket, Env) of
		message_error ->
		    Proxy ! {message_error, self(), ReqId},
		    ok;
		_ ->
		    ok
	    end;
	ok ->
	    ok;
	{'EXCEPTION', E} ->
	    Proxy ! {message_error, self(), ReqId},
	    Reply = marshal_exception(Env, ReqId, E, enc_reply),
	    orber_socket:write(SocketType, Socket, Reply)
    end.



collect(_Proxy, _Buffer, _ByteOrder, _ReqId, MaxFrags, MaxFrags) ->
    orber:dbg("[~p] ~p:collect(~p)~nMax fragments limit reached.", 
	      [?LINE, ?MODULE, MaxFrags], ?DEBUG_LEVEL),
    {'EXCEPTION', #'IMP_LIMIT'{completion_status=?COMPLETED_NO}};
collect(Proxy, Buffer, ByteOrder, ReqId, MaxFrags, FragCounter) ->
    receive
        {Proxy, #giop_message{byte_order = ByteOrder,
			      message    = Message,
			      fragments  = true} = GIOPHdr} ->
	    {_, #fragment_header{request_id=ReqId}, FragBody, _, _} = 
		cdr_decode:dec_message_header(null, GIOPHdr, Message),
            collect(Proxy, [FragBody | Buffer], ByteOrder, ReqId, 
		    MaxFrags, FragCounter+1);
	{Proxy, #giop_message{byte_order = ByteOrder,
			      message = Message,
			      fragments = false} = GIOPHdr} ->
	    {_, #fragment_header{request_id=ReqId}, FragBody, _, _} = 
		cdr_decode:dec_message_header(null, GIOPHdr, Message),
	    {ok, lists:reverse([FragBody | Buffer])};
	{Proxy, GIOPHdr, _Data, _} ->
	    orber:dbg("[~p] orber_iiop_inrequest:collect(~p, ~p)~n"
		      "Incorrect Fragment. Might be different byteorder.", 
		      [?LINE, ByteOrder, GIOPHdr], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'MARSHAL'{completion_status=?COMPLETED_NO}};
	{Proxy, cancel_request_header} ->
	    ok;
	Other ->
	    orber:dbg("[~p] ~p:collect(~p)~n"
		      "Unable to collect all fragments: ~p", 
		      [?LINE, ?MODULE, Buffer, Other], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'MARSHAL'{completion_status=?COMPLETED_NO}}
    end.


%%-----------------------------------------------------------------
%% Func: handle_message/4
%%-----------------------------------------------------------------
handle_message(GIOPHdr, Message, SocketType, Socket, Env) ->
    %% Warning. We shouldn't set the flags like this here. But, for now, we'll
    %% do it due to performance reasons.
    put(oe_orber_flags, Env#giop_env.flags),
    case catch cdr_decode:dec_message_header(null, GIOPHdr, Message) of
	Hdr when is_record(Hdr, cancel_request_header) ->
            %% We just skips this message for the moment, the standard require that 
	    %% the client handles the reply anyway.
	    message_error;
	{location_forward, Object, ReqId, Version, OldObj} ->
	    Reply = call_interceptors_out(Env#giop_env{version = Version}, 
					  ReqId, [Object], OldObj,
					  'location_forward', 
					  "location_forward",
					  {{'tk_objref', "", ""}, [],[]}),
	    orber_socket:write(SocketType, Socket, Reply);
	{object_forward, Object, ReqId, Version, _OldObj} ->
	    Reply = handle_locate_request(Env#giop_env{version = Version}, 
					  {object_forward, Object, ReqId}),
	    orber_socket:write(SocketType, Socket, Reply);
	{Version, Hdr} when is_record(Hdr, locate_request_header) ->
	    Reply = handle_locate_request(Env#giop_env{version = Version}, Hdr),
	    orber_socket:write(SocketType, Socket, Reply);
	{Version, ReqHdr, Rest, Len, ByteOrder} when is_record(ReqHdr, request_header) ->
	    handle_request(Env#giop_env{version = Version}, ReqHdr, Rest, Len, 
			   ByteOrder, SocketType, Socket, Message);
	Other ->
	    %% This cluase takes care of all erranous messages.
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)~n"
		      "Decoding Msg Header failed: ~p", 
		      [?LINE, Message, Other], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(Env),
	    orber_socket:write(SocketType, Socket, Reply),
	    message_error
   end.


send_reply(oneway, _SocketType, _Socket) ->
    ok;
send_reply(Reply, SocketType, Socket) ->
    orber_socket:write(SocketType, Socket, Reply).

%%-----------------------------------------------------------------
%% Func: handle_request
%%-----------------------------------------------------------------
handle_request(#giop_env{interceptors = false} = Env, ReqHdr, Rest, Len, ByteOrder, 
	       SocketType, Socket, Message) ->
    NewEnv = check_context(ReqHdr#request_header.service_context, [], Env),
    case decode_body(NewEnv, ReqHdr, Rest, Len, ByteOrder, Message, enc_reply) of
	{error, E} ->
	    orber_socket:write(SocketType, Socket, E);
	{NewEnv2, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes, Env),
	    Reply  = evaluate(NewEnv2, Hdr, Result, TypeCodes, 
			      enc_reply, 'no_exception'),
	    send_reply(Reply, SocketType, Socket)
    end;
handle_request(Env, ReqHdr, Rest, Len, ByteOrder, SocketType, Socket, Message) ->
    NewEnv = check_context(ReqHdr#request_header.service_context, [], Env),
    case catch call_interceptors(SocketType, NewEnv, ReqHdr, 
				 Rest, Len, ByteOrder, Message) of
	{error, E} ->
	    %% Failed to decode body.
	    orber_socket:write(SocketType, Socket, E);
	{'EXCEPTION', Exc} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)~n"
		      "Invoking the interceptors resulted in: ~p", 
		      [?LINE, Message, Exc], ?DEBUG_LEVEL),
	    Reply = marshal_exception(NewEnv, 
				      ReqHdr#request_header.request_id, 
				      Exc, enc_reply),
	    orber_socket:write(SocketType, Socket, Reply);
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)~n"
		      "Invoking the interceptors resulted in: ~p", 
		      [?LINE, ReqHdr, R], ?DEBUG_LEVEL),
	    Reply = marshal_exception(NewEnv, 
				      ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_MAYBE}, 
				      enc_reply),
	    orber_socket:write(SocketType, Socket, Reply);
	Reply ->
	    send_reply(Reply, SocketType, Socket)
    end.

check_context([], [], Env) ->
    Env;
check_context([], Acc, Env) ->
    Env#giop_env{ctx = Acc};
check_context([#'CSI_SASContextBody'
	       {label = ?CSI_MsgType_MTEstablishContext, 
		value = #'CSI_EstablishContext'
		{client_context_id = _Id, 
		 authorization_token = _AuthToken,
		 identity_token = _IdToken, 
		 client_authentication_token = _CAuthToken}}|Rest], Acc, Env) ->
    check_context(Rest, [#'IOP_ServiceContext'
			 {context_id=?IOP_SecurityAttributeService,
			  context_data = #'CSI_SASContextBody'
			  {label = ?CSI_MsgType_MTCompleteEstablishContext, 
			   value = #'CSI_CompleteEstablishContext'
			   {client_context_id = 0, 
			    context_stateful = false,
			    final_context_token = [0,255]}}}|Acc], Env);
check_context([_|Rest], Acc, Env) ->
    check_context(Rest, Acc, Env).
    

%%-----------------------------------------------------------------
%% Func: call_interceptors
%%-----------------------------------------------------------------
call_interceptors(SocketType, #giop_env{interceptors = {native, Ref, PIs},
					ctx = Ctx} = Env, 
		  ReqHdr, Rest, Len, ByteOrder, Msg) ->
    NewRest = orber_pi:in_request_enc(PIs, ReqHdr, Ref, Rest),
    case decode_body(Env, ReqHdr, NewRest, Len, ByteOrder, Msg, enc_reply) of
	{NewEnv, Hdr, Par, TypeCodes} ->
	    NewPar = orber_pi:in_request(PIs, ReqHdr, Ref, Par),
	    ResultInv = invoke_request(Hdr, NewPar, SocketType, TypeCodes, NewEnv),
	    Result = orber_pi:out_reply(PIs, ReqHdr, Ref, ResultInv, Ctx),
	    
            case evaluate(NewEnv, ReqHdr, Result, TypeCodes, enc_reply_split,
			  'no_exception') of
		{ReplyHdr, Reply, HdrL, _BodyL, Flags} ->
		    NewReply = orber_pi:out_reply_enc(PIs, ReqHdr, Ref, Reply, Ctx),
		    MessSize = HdrL+size(NewReply),
		    cdr_encode:enc_giop_message_header(NewEnv, 'reply', Flags, 
						       MessSize, [ReplyHdr|NewReply]);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end;
call_interceptors(SocketType, #giop_env{interceptors = {portable, _PIs}} = Env, 
		  ReqHdr, Rest, Len, ByteOrder, Msg) ->
    case decode_body(Env, ReqHdr, Rest, Len, ByteOrder, Msg, enc_reply) of
	{NewEnv, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes, NewEnv),
	    evaluate(NewEnv, ReqHdr, Result, TypeCodes, enc_reply, 'no_exception');
	Other ->
	    Other
    end.

%%-----------------------------------------------------------------
%% Func: call_interceptors_out
%%-----------------------------------------------------------------
call_interceptors_out(#giop_env{interceptors = {native, Ref, PIs}, ctx = Ctx} = Env, 
		      ReqId, Result, Obj, Type, Operation, TypeCodes) ->
    ReqHdr = #request_header{object_key = Obj,
			     service_context = Ctx,
			     response_expected = true,
			     request_id = ReqId,
			     operation = Operation},
    NewResult = (catch orber_pi:out_reply(PIs, ReqHdr, Ref, Result, Ctx)),
    {ReplyHdr, Reply, HdrL, _BodyL, Flags} = 
	evaluate(Env, ReqHdr, NewResult, TypeCodes, enc_reply_split, Type),
    NewReply = 
	case catch orber_pi:out_reply_enc(PIs, ReqHdr, Ref, Reply, Ctx) of
	    {'EXCEPTION', Exception} ->
		%% Since evaluate don't need TypeCodes or Status no need to supply
		%% them.
		evaluate(Env, ReqHdr, {'EXCEPTION', Exception}, undefined, 
			 enc_reply_split, undefined);
	    {'EXIT', E} ->
		orber:dbg("[~p] orber_iiop_inrequest:handle_location_forward(~p)~n"
			  "Resulted in exit: ~p", [?LINE, PIs, E], ?DEBUG_LEVEL),
		marshal_exception(Env, ReqId,
				  #'MARSHAL'{completion_status=?COMPLETED_NO}, 
				  enc_reply);
	    R ->
		R
	end,
    MessSize = HdrL+size(NewReply),
    cdr_encode:enc_giop_message_header(Env, 'reply', Flags, MessSize, 
				       [ReplyHdr|NewReply]);
call_interceptors_out(#giop_env{interceptors = {portable, _PIs}} = Env,
		      ReqId, Result, _Obj, Type, _, TypeCodes) ->
    Hdr = #request_header{response_expected = true,
			  request_id = ReqId},
    evaluate(Env, Hdr, Result, TypeCodes, enc_reply, Type);
call_interceptors_out(Env, ReqId, Result, _Obj, Type, _, TypeCodes) ->
    Hdr = #request_header{response_expected = true,
			  request_id = ReqId},
    evaluate(Env, Hdr, Result, TypeCodes, enc_reply, Type).


%%-----------------------------------------------------------------
%% Func: decode_body/2
%%-----------------------------------------------------------------
decode_body(#giop_env{version = Version} = Env, ReqHdr, Rest, Len, 
	    ByteOrder, Message, Func) ->
    case catch cdr_decode:dec_request_body(Version, ReqHdr, Rest, Len, 
					   ByteOrder, Message) of
	{NewVersion, ReqHdr, Par, TypeCodes} ->
	    {Env#giop_env{version = NewVersion}, ReqHdr, Par, TypeCodes};
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:decode_body(~p, ~p)~n"
		      "Failed decoding request body: ~p", 
		      [?LINE, ReqHdr, Message, E], ?DEBUG_LEVEL),
	    {error, marshal_exception(Env, ReqHdr#request_header.request_id,
				      E, Func)};
	Other ->
	    %% This cluase takes care of all erranous messages.
	    orber:dbg("[~p] orber_iiop_inrequest:decode_body(~p, ~p)~n"
		      "Failed decoding request body: ~p", 
		      [?LINE, ReqHdr, Message, Other], ?DEBUG_LEVEL),
	    {error, marshal_exception(Env, ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_NO},
				      Func)}
    end.


%%-----------------------------------------------------------------
%% Func: handle_locate_request/2
%%-----------------------------------------------------------------
handle_locate_request(Env, {object_forward, Object, ReqId}) ->
    case catch cdr_encode:enc_locate_reply(
		 Env#giop_env{request_id = ReqId,
			      tc = {'tk_objref', "", ""},
			      result = Object,
			      reply_status = 'object_forward'}) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(object_forward)~n"
		      "Raised the exception: ~p", [?LINE, Exception], ?DEBUG_LEVEL),
	    marshal_locate_exception(Env, ReqId, Exception);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(object_forward)~n"
		      "Resulted in exit: ~p", [?LINE, E], ?DEBUG_LEVEL),
	    marshal_locate_exception(Env, ReqId,
				     #'MARSHAL'{completion_status=?COMPLETED_NO});
	R ->
	    R
    end;
handle_locate_request(Env, Hdr) ->
    Location = orber_objectkeys:check(Hdr#locate_request_header.object_key),
    case catch cdr_encode:enc_locate_reply(
		 Env#giop_env{request_id = Hdr#locate_request_header.request_id,
			      reply_status = Location}) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(~p)~n"
		      "Raised the exception: ~p", 
		      [?LINE, Location, Exception], ?DEBUG_LEVEL),
	    marshal_locate_exception(Env, Hdr#locate_request_header.request_id, Exception);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(~p)~n"
		      "Resulted in exit: ~p", [?LINE, Location, E], ?DEBUG_LEVEL),
	    marshal_locate_exception(Env, Hdr#locate_request_header.request_id,
				     #'MARSHAL'{completion_status=?COMPLETED_NO});
	R ->
	    R
    end.
  
%%-----------------------------------------------------------------
%% Func: invoke_request/2
%%-----------------------------------------------------------------
invoke_request(Hdr, Par, normal, TypeCodes, #giop_env{iiop_ssl_port = SSLPort, 
						      partial_security = PartialSec}) ->
    Result = 
	case SSLPort of
	    -1 ->
		corba:request_from_iiop(Hdr#request_header.object_key,
					Hdr#request_header.operation,
					Par, [], Hdr#request_header.response_expected,
					Hdr#request_header.service_context);
	    _ ->
		case Hdr#request_header.object_key of
		    {_,registered,orber_init,_,_,_} ->
			corba:request_from_iiop(Hdr#request_header.object_key,
						Hdr#request_header.operation,
						Par, [], 
						Hdr#request_header.response_expected,
						Hdr#request_header.service_context);
		    {_,_,_,_,_,Flags} when PartialSec == true, 
					   ?ORB_FLAG_TEST(Flags, ?ORB_NO_SECURITY) == true ->
			corba:request_from_iiop(Hdr#request_header.object_key,
						Hdr#request_header.operation,
						Par, [], 
						Hdr#request_header.response_expected,
						Hdr#request_header.service_context);
		    _ ->
			orber:dbg("[~p] orber_iiop_inrequest:invoke_request(~p)~n"
				  "SSL do not permit", 
				  [?LINE, Hdr#request_header.object_key], ?DEBUG_LEVEL),
			{'EXCEPTION', #'NO_PERMISSION'{completion_status=?COMPLETED_NO}}
		end
	end,
    result_to_list(Result, TypeCodes);	
invoke_request(Hdr, Par, ssl, TypeCodes, _) ->
    Result = corba:request_from_iiop(Hdr#request_header.object_key,
				     Hdr#request_header.operation,
				     Par, [], Hdr#request_header.response_expected,
				     Hdr#request_header.service_context),
    result_to_list(Result, TypeCodes).

%%-----------------------------------------------------------------
%% Func: evaluate/4
%%-----------------------------------------------------------------
evaluate(_, Hdr,_,_,_,_) when Hdr#request_header.response_expected == 'false' ->
    oneway;
evaluate(Env, Hdr, _, _, Func, _) 
  when Hdr#request_header.response_expected == 'true_oneway' ->
    %% Special case which only occurs when using IIOP-1.2
    cdr_encode:Func(Env#giop_env{request_id = Hdr#request_header.request_id,
				 reply_status = 'no_exception', 
				 tc = {tk_null,[],[]}, result = null});
evaluate(Env, Hdr, {'EXCEPTION', Exc}, _, Func, _) ->
    %% The exception can be user defined. Hence, we must check the result.
    case catch marshal_exception(Env, Hdr#request_header.request_id, Exc, Func) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)~n"
		      "Encoding (reply) exception: ~p", 
		      [?LINE, Hdr, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Env, Hdr#request_header.request_id, Exception, Func);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)~n"
		      "Encode (reply) resulted in: ~p", 
		      [?LINE, Hdr, E], ?DEBUG_LEVEL),
	    marshal_exception(Env, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func);
	R ->
	    R	
    end;
evaluate(#giop_env{version = {1,2}} = Env, Hdr, {'location_forward_perm', NewIOR}, _, 
	 Func, _)->
    case catch cdr_encode:Func(#giop_env{version = {1,2},
					 request_id = Hdr#request_header.request_id,
					 reply_status = 'location_forward_perm',
					 tc = {{'tk_objref', "", ""}, [],[]},
					 result = NewIOR}) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p) " ++
		      "Encoding (reply) exception: ~p",
		      [?LINE, Hdr, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Env, Hdr#request_header.request_id, Exception, Func);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p) " ++
		      "Encode (reply) resulted in: ~p", 
		      [?LINE, Hdr, E], ?DEBUG_LEVEL),
	    marshal_exception(Env, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func);
	R ->
	    R
    end;
evaluate(Env, Hdr, [Res |OutPar], TypeCodes, Func, Type) ->
    case catch cdr_encode:Func(Env#giop_env{request_id = Hdr#request_header.request_id,
					    reply_status = Type,
					    tc = TypeCodes, result = Res, 
					    parameters = OutPar}) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p, ~p, ~p)~n"
		      "Encode exception: ~p", 
		      [?LINE, Hdr, Res, OutPar, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Env, Hdr#request_header.request_id, Exception, Func);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p, ~p, ~p)~n"
		      "Encode exit: ~p", 
		      [?LINE, Hdr, Res, OutPar, E], ?DEBUG_LEVEL),
	    marshal_exception(Env, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func);
	R ->
	    R
    end;
evaluate(Env, Hdr, What, TypeCodes, Func, _) ->
    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)~n"
	      "Bad reply: ~p~n"
	      "Should be: ~p~n"
	      "GIOP Env : ~p", [?LINE, Hdr, What, TypeCodes, Env], ?DEBUG_LEVEL),
    marshal_exception(Env, Hdr#request_header.request_id, 
		      #'INTERNAL'{completion_status=?COMPLETED_MAYBE}, Func).

%%-----------------------------------------------------------------
%% Utility Functions
%%-----------------------------------------------------------------
result_to_list({'oe_location_forward_perm', NewIOR}, _) ->
    {'location_forward_perm', NewIOR};
result_to_list({'EXCEPTION', E}, _) ->
    {'EXCEPTION', E};
result_to_list(Result, {_TkRes, _, []}) ->
    [Result];
result_to_list(Result, {_TkRes, _, _TkOut}) ->
    tuple_to_list(Result).

marshal_exception(Env, Id, Exception, Func) ->
    {TypeOfException, ExceptionTypeCode, NewExc} =
	orber_exceptions:get_def(Exception),
    cdr_encode:Func(Env#giop_env{request_id = Id, 
				 reply_status = TypeOfException, 
				 tc = {ExceptionTypeCode, [], []}, 
				 result = NewExc}).

marshal_locate_exception(#giop_env{version = {1,2}} = Env, Id, Exception) ->
    case orber_exceptions:get_def(Exception) of
	{?SYSTEM_EXCEPTION, ExceptionTypeCode, NewExc} ->
	    cdr_encode:enc_locate_reply(
	      Env#giop_env{request_id = Id,
			   reply_status = 'loc_system_exception', 
			   tc = ExceptionTypeCode, result = NewExc});
	_ ->
	    %% This case is impossible (i.e. Orber only throws system
	    %% exceptions). But to be on the safe side...
	    marshal_locate_exception(Env, Id, #'MARSHAL'
				     {completion_status=?COMPLETED_YES})
    end;
marshal_locate_exception(Env, _Id, _Exception) ->
    %% There is no way to define an exception for IIOP-1.0/1.1 in a 
    %% locate_reply.
    cdr_encode:enc_message_error(Env).
