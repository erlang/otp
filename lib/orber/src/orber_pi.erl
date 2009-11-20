%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : orber_pi.erl
%% Purpose : 
%% Comments:
%% * Each Interceptor is represented by Module where
%%              Module - refers to a module which must export the functions:
%%                       (1)  receive_request
%%                       (2)  send_other
%%                       (3)  receive_service_contexts
%%                       (4)  send_reply
%%                       (5)  send_exception
%%                       (6)  send_request
%%                       (7)  send_poll
%%                       (8)  receive_reply
%%                       (9)  receive_exception
%%                       (10) receive_other 
%%                       or
%%                       (11) new_out_connection
%%                       (12) new_in_connection
%%                       (13) in_request
%%                       (14) out_reply
%%                       (15) out_request
%%                       (16) in_reply
%%
%%              Functions (1) - (10) for Portable and (11) - (16) for 
%%              Native Interceptors.
%% 
%%----------------------------------------------------------------------

-module(orber_pi).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/orber_pi.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%--------------- EXPORTS-------------------------------------
%% API external
-export([%% Native Intercepotors API
	 new_out_connection/5,
	 new_in_connection/5,
	 closed_in_connection/2,
	 closed_out_connection/2,
	 in_request_enc/4,
	 out_reply_enc/5,
	 out_request_enc/6,
	 in_reply_enc/6,
	 in_request/4,
	 out_reply/5,
	 out_request/6,
	 in_reply/6,
	 %% Portable Interceptors
	 server_start_receive/7, 
	 server_start_send/2,
	 client_receive/2, 
	 client_send/2, 
	 codefactory_create_codec/1,
	 codec_encode/2, 
	 codec_encode_value/2,
	 codec_decode/2, 
	 codec_decode_value/3,
	 %% RequestInfo
	 '_get_request_id'/1,
	 '_get_operation'/1,
	 '_get_arguments'/1,
	 '_get_exceptions'/1,
	 '_get_contexts'/1,
	 '_get_operation_context'/1,
	 '_get_result'/1,
	 '_get_response_expected'/1,
	 '_get_sync_scope'/1,
	 '_get_reply_status'/1,
	 '_get_forward_reference'/1,
	 get_slot/2,
	 get_request_service_context/2,
	 get_reply_service_context/2,
	 %% ClientRequestInfo (inherrits RequestInfo)
	 '_get_target'/1,
	 '_get_effective_target'/1,
	 '_get_effective_profile'/1,
	 '_get_received_exception'/1,
	 '_get_received_exception_id'/1,
	 get_effective_component/2,
	 get_effective_components/2,
	 get_request_policy/2,
	 add_request_service_policy/3,
	 %% ServerRequestInfo (inherrits RequestInfo)
	 '_get_sending_exception'/1,
	 '_get_object_id'/1,
	 '_get_adapter_id'/1,
	 '_get_target_most_derived_interface'/1,
	 get_server_policy/2,
	 set_slot/3,
	 target_is_a/2,
	 add_reply_service_context/3]).

%%=============== DATA STRUCTURES ============================
%%--------------- ClientRequestInfo --------------------------
-record('ClientRequestInfo', 
	{request_id,
	 operation,
	 arguments,
	 exceptions,
	 contexts,
	 operation_context,
	 result,
	 response_expected,
	 sync_scope = 'SYNC_NONE',
	 reply_status,
	 forward_reference,
	 endian,
	 target,
	 effective_target,
	 effective_profile,
	 received_exception,
	 received_exception_id}).

-define(createInitCRI(_ReqID, _Op, _Args, _Ctxs, _OpCtx, _RespExp, _Target, 
		      _ETarget, _EProf),
	#'ClientRequestInfo'{request_id        = _ReqID,
			     operation         = _Op,
			     arguments         = _Args,
			     contexts          = _Ctxs,
			     operation_context = _OpCtx,
			     response_expected = _RespExp,
			     target            = _Target,
			     effective_target  = _ETarget,
			     effective_profile = _EProf}).


%%--------------- ServerRequestInfo --------------------------
-record('ServerRequestInfo',
	{request_id,
	 operation,
	 arguments,
	 exceptions,
	 contexts,
	 operation_context,
	 result,
	 response_expected,
	 sync_scope = 'SYNC_NONE',
	 reply_status,
	 forward_reference,
	 endian,
	 sending_exception,
	 object_id,
	 adapter_id,
	 target_most_derived_interface}).

-define(createInitSRI(_ReqID, _Op, _RespExp),
	#'ServerRequestInfo'{request_id        = _ReqID,
			     operation         = _Op,
			     response_expected = _RespExp}).


%%--------------- DEFINES ------------------------------------
-define(DEBUG_LEVEL, 9).

-define(EFORMAT(_F, _A), exit(lists:flatten(io_lib:format(_F, _A)))).

%%------------------------------------------------------------
%%------------- NATIVE INTERCEPTOR FUNCTIONS------------------
%%------------------------------------------------------------
%% function : new_in_connection
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
new_in_connection(PIs, Host, Port, SHost, SPort) ->
    case catch new_in_connection(PIs, undefined, Host, Port, SHost, SPort) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:new_in_connection(~p); exit(~p)", 
		      [?LINE, PIs, R], ?DEBUG_LEVEL),
	    ?EFORMAT("Supplied Interceptors unable to create a valid new_in_connection"
		     "Reason: ~p", [{'EXIT', R}]);
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:new_in_connection(~p); exception(~p)", 
		      [?LINE, PIs, E], ?DEBUG_LEVEL),
	    ?EFORMAT("Supplied Interceptors unable to create a valid new_in_connection"
		     "Reason: ~p", [{'EXCEPTION', E}]);
	Ref ->
	    Ref
    end.

new_in_connection([], Ref, _, _, _, _) ->
    Ref;
new_in_connection([Mod|T], Ref, Host, Port, SHost, SPort) ->
    case get_arity(Mod, new_in_connection) of
	5 ->
	    NewRef = Mod:new_in_connection(Ref, Host, Port, SHost, SPort),
	    new_in_connection(T, NewRef, Host, Port, SHost, SPort);
	3 ->
	    NewRef = Mod:new_in_connection(Ref, Host, Port),
	    new_in_connection(T, NewRef, Host, Port, SHost, SPort)
    end.

get_arity(Mod, Func) ->
    get_arity(Mod, Func, true).
get_arity(Mod, Func, Retry) ->
    case erlang:function_exported(Mod, Func, 5) of
	true ->
	    5;
	false ->
	    case erlang:function_exported(Mod, Func, 3) of
		true ->
		    3;
		false when Retry == true ->
		    {module, _} = code:ensure_loaded(Mod),
		    get_arity(Mod, Func, false);
		false ->
		    exit("Unable to load interceptor")
	    end
    end.

%%------------------------------------------------------------
%% function : closed_in_connection
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
closed_in_connection(PIs, Ref) ->
    case catch closed_in_connection_helper(PIs, Ref) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:closed_in_connection(~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, R], ?DEBUG_LEVEL),
	    ok;
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:closed_in_connection(~p, ~p); exception(~p)", 
		      [?LINE, PIs, Ref, E], ?DEBUG_LEVEL),
	    ok;
	_ ->
	    ok
    end.

closed_in_connection_helper([], _Ref) ->
    ok;
closed_in_connection_helper([Mod|T], Ref) ->
    NewRef = Mod:closed_in_connection(Ref),
    closed_in_connection_helper(T, NewRef).


%%------------------------------------------------------------
%% function : new_out_connection
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
new_out_connection(PIs, Host, Port, SHost, SPort) ->
    case catch new_out_connection(PIs, undefined, Host, Port, SHost, SPort) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:new_out_connection(~p); exit(~p)", 
		      [?LINE, PIs, R], ?DEBUG_LEVEL),
	    ?EFORMAT("Supplied Interceptors unable to create a valid new_out_connection"
		     "Reason: ~p", [{'EXIT', R}]);
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:new_out_connection(~p); exception(~p)", 
		      [?LINE, PIs, E], ?DEBUG_LEVEL),
	    ?EFORMAT("Supplied Interceptors unable to create a valid new_out_connection"
		     "Reason: ~p", [{'EXCEPTION', E}]);
	Ref ->
	    Ref
    end.

new_out_connection([], Ref, _, _, _, _) ->
    Ref;
new_out_connection([Mod|T], Ref, Host, Port, SHost, SPort) ->
    case get_arity(Mod, new_out_connection) of
	5 ->
	    NewRef = Mod:new_out_connection(Ref, Host, Port, SHost, SPort),
	    new_out_connection(T, NewRef, Host, Port, SHost, SPort);
	3 ->
	    NewRef = Mod:new_out_connection(Ref, Host, Port),
	    new_out_connection(T, NewRef, Host, Port, SHost, SPort)
    end.

%%------------------------------------------------------------
%% function : closed_out_connection
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
closed_out_connection(PIs, Ref) ->
    case catch closed_out_connection_helper(PIs, Ref) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:closed_out_connection(~p); exit(~p)", 
		      [?LINE, PIs, R], ?DEBUG_LEVEL),
	    ok;
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:closed_out_connection(~p); exception(~p)", 
		      [?LINE, PIs, E], ?DEBUG_LEVEL),
	    ok;
	_ ->
	    ok
    end.

closed_out_connection_helper([], _Ref) ->
    ok;
closed_out_connection_helper([Mod|T], Ref) ->
    NewRef = Mod:closed_out_connection(Ref),
    closed_out_connection_helper(T, NewRef).

%%------------------------------------------------------------
%% function : in_request_enc
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercepts an incoming request (server-side).
%%------------------------------------------------------------
in_request_enc(PIs, ReqHdr, Ref, Msg) ->
    case catch in_request_enc(PIs, ReqHdr, Ref, Msg, undefined) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:in_request_enc(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:in_request_enc(~p, ~p, ~p); exception(~p)", 
		      [?LINE, PIs, Ref, Msg, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	NewMsg ->
	    NewMsg
    end.

in_request_enc([], _, _, Msg, _) ->
    Msg;
in_request_enc([Mod|T], ReqHdr, Ref, Msg, Args) ->
    {NewMsg, NewArgs} = Mod:in_request_encoded(Ref, ReqHdr#request_header.object_key,
					       ReqHdr#request_header.service_context,
					       ReqHdr#request_header.operation, 
					       Msg, Args),
    in_request_enc(T, ReqHdr, Ref, NewMsg, NewArgs).

%%------------------------------------------------------------
%% function : in_request
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercepts an incoming request (server-side).
%%------------------------------------------------------------
in_request(PIs, ReqHdr, Ref, Msg) ->
    case catch in_request(PIs, ReqHdr, Ref, Msg, undefined) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:in_request(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:in_request(~p, ~p, ~p); exception(~p)", 
		      [?LINE, PIs, Ref, Msg, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	NewMsg ->
	    NewMsg
    end.

in_request([], _, _, Msg, _) ->
    Msg;
in_request([Mod|T], ReqHdr, Ref, Msg, Args) ->
    {NewMsg, NewArgs} = Mod:in_request(Ref, ReqHdr#request_header.object_key,
				       ReqHdr#request_header.service_context,
				       ReqHdr#request_header.operation, 
				       Msg, Args),
    in_request(T, ReqHdr, Ref, NewMsg, NewArgs).

%%------------------------------------------------------------
%% function : out_reply_enc
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercept an outgoing reply (server-side).
%%------------------------------------------------------------
out_reply_enc(PIs, ReqHdr, Ref, Msg, Ctx) ->
    case catch out_reply_enc(PIs, ReqHdr, Ref, Msg, undefined, Ctx) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:out_reply_enc(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:out_reply_enc(~p, ~p, ~p); exception(~p)", 
		      [?LINE, PIs, Ref, Msg, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	NewMsg ->
	    NewMsg
    end.
out_reply_enc([], _, _, Msg, _, _) ->
    Msg;
out_reply_enc([Mod|T], ReqHdr, Ref, Msg, Args, Ctx) ->
    {NewMsg, NewArgs} = Mod:out_reply_encoded(Ref, ReqHdr#request_header.object_key,
					      Ctx, %% Out Context.
					      ReqHdr#request_header.operation,
					      Msg, Args),
    out_reply_enc(T, ReqHdr, Ref, NewMsg, NewArgs, Ctx).


%%------------------------------------------------------------
%% function : out_reply
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercept an outgoing reply (server-side).
%%------------------------------------------------------------
out_reply(PIs, ReqHdr, Ref, Msg, Ctx) ->
    case catch out_reply(PIs, ReqHdr, Ref, Msg, undefined, Ctx) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:out_reply(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	NewMsg ->
	    NewMsg
    end.
out_reply([], _, _, Msg, _, _) ->
    Msg;
out_reply([Mod|T], ReqHdr, Ref, Msg, Args, Ctx) ->
    {NewMsg, NewArgs} = Mod:out_reply(Ref, ReqHdr#request_header.object_key,
				      Ctx, %% Out Context.
				      ReqHdr#request_header.operation,
				      Msg, Args),
    out_reply(T, ReqHdr, Ref, NewMsg, NewArgs, Ctx).


%%------------------------------------------------------------
%% function : out_request_enc
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercept an outgoing request (client-side).
%%------------------------------------------------------------
out_request_enc(PIs, ObjKey, Ctx, Op, Ref, Msg) ->
    case catch out_request_enc(PIs, ObjKey, Ctx, Op, Ref, Msg, undefined) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:out_request_enc(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:out_request_enc(~p, ~p, ~p); exception(~p)", 
		      [?LINE, PIs, Ref, Msg, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	NewMsg ->
	    NewMsg
    end.

out_request_enc([], _, _, _, _, Msg, _) ->
    Msg;
out_request_enc([Mod|T], ObjKey, Ctx, Op, Ref, Msg, Args) ->
    {NewMsg, NewArgs} = Mod:out_request_encoded(Ref, ObjKey, Ctx, Op, Msg, Args),
    out_request_enc(T, ObjKey, Ctx, Op, Ref, NewMsg, NewArgs).


%%------------------------------------------------------------
%% function : out_request
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercept an outgoing request (client-side).
%%------------------------------------------------------------
out_request(PIs, ObjKey, Ctx, Op, Ref, Msg) ->
    case catch out_request(PIs, ObjKey, Ctx, Op, Ref, Msg, undefined) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:out_request(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:out_request(~p, ~p, ~p); exception(~p)", 
		      [?LINE, PIs, Ref, Msg, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	NewMsg ->
	    NewMsg
    end.

out_request([], _, _, _, _, Msg, _) ->
    Msg;
out_request([Mod|T], ObjKey, Ctx, Op, Ref, Msg, Args) ->
    {NewMsg, NewArgs} = Mod:out_request(Ref, ObjKey, Ctx, Op, Msg, Args),
    out_request(T, ObjKey, Ctx, Op, Ref, NewMsg, NewArgs).


%%------------------------------------------------------------
%% function :in_reply_enc
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercept an incoming reply (client-side)
%%------------------------------------------------------------
in_reply_enc(PIs, ObjKey, Ctx, Op, Ref, Msg) ->
    case catch in_reply_enc(PIs, ObjKey, Ctx, Op, Ref, Msg, undefined) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:in_reply_enc(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_pi:in_reply_enc(~p, ~p, ~p); exception(~p)", 
		      [?LINE, PIs, Ref, Msg, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	NewMsg ->
	    NewMsg
    end.

in_reply_enc([], _, _, _, _, Msg, _) ->
    Msg;
in_reply_enc([Mod|T], ObjKey, Ctx, Op, Ref, Msg, Args) ->
    {NewMsg, NewArgs} = Mod:in_reply_encoded(Ref, ObjKey, Ctx, Op, Msg, Args),
    in_reply_enc(T, ObjKey, Ctx, Op, Ref, NewMsg, NewArgs).

%%------------------------------------------------------------
%% function :in_reply
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : Intercept an incoming reply (client-side)
%%------------------------------------------------------------
in_reply(PIs, ObjKey, Ctx, Op, Ref, Msg) ->
    case catch in_reply(PIs, ObjKey, Ctx, Op, Ref, Msg, undefined) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_pi:in_reply(~p, ~p, ~p); exit(~p)", 
		      [?LINE, PIs, Ref, Msg, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	NewMsg ->
	    NewMsg
    end.

in_reply([], _, _, _, _, Msg, _) ->
    Msg;
in_reply([Mod|T], ObjKey, Ctx, Op, Ref, Msg, Args) ->
    {NewMsg, NewArgs} = Mod:in_reply(Ref, ObjKey, Ctx, Op, Msg, Args),
    in_reply(T, ObjKey, Ctx, Op, Ref, NewMsg, NewArgs).




%%------------------------------------------------------------
%%------------- CODEC FUNCTIONS ------------------------------
%%------------------------------------------------------------
%% function : codefactory_create_codec
%% Arguments: #IOP_N_Encoding{}
%% Returns  : CodecRef
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codefactory_create_codec(#'IOP_N_Encoding'{format = 'IOP_N_ENCODING_CDR_ENCAPS', 
					   major_version = Major, 
					   minor_version = Minor}) 
  when is_integer(Major) andalso is_integer(Minor) ->
    {Major, Minor};
codefactory_create_codec(_) ->
    corba:raise(#'IOP_N_CodecFactory_UnknownEncoding'{}).

%%------------------------------------------------------------
%% function : codec_encode
%% Arguments: Version - GIOP version
%%            Any - #any{}
%% Returns  : CORBA::OctetSeq
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codec_encode(Version, Any) when is_record(Any, any) ->
    %% Encode ByteOrder
    {Bytes, Len} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes2, _Len2} = cdr_encode:enc_type('tk_any', Version, Any, Bytes, Len),
    list_to_binary(lists:reverse(Bytes2));
codec_encode(_Version, _Any) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : codec_encode_value
%% Arguments: Version - GIOP version
%%            Any - #any{}
%% Returns  : CORBA::OctetSeq
%% Exception: 
%% Effect   : Encode the Any#any.value only.
%%------------------------------------------------------------
codec_encode_value(Version, #any{typecode = TC, value = Val}) ->
    %% Encode ByteOrder
    {Bytes, Len} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes2, _Len2} = cdr_encode:enc_type(TC, Version, Val, Bytes, Len),
    list_to_binary(lists:reverse(Bytes2));
codec_encode_value(_Version, _NotAnAny) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : codec_decode
%% Arguments: Version - GIOP version
%%            Bytes - CORBA::OctetSeq
%% Returns  : Any - #any{}
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codec_decode(Version, Bytes) when is_binary(Bytes) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(Bytes),
    case catch cdr_decode:dec_type('tk_any', Version, Rest, 0, ByteOrder) of
	{Any, [], _} ->
	    Any;
	_->
	    corba:raise(#'IOP_N_Codec_FormatMismatch'{})
    end;
codec_decode(_Version, _Any) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : codec_decode_value
%% Arguments: Version - GIOP version
%%            Bytes - CORBA::OctetSeq
%%            TypeCode - CORBA::TypeCode
%% Returns  : Any - #any{}
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codec_decode_value(Version, Bytes, TypeCode) when is_binary(Bytes) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(Bytes),
    case catch cdr_decode:dec_type(TypeCode, Version, Rest, 0, ByteOrder) of
	{Val, [], _} ->
	    #any{typecode = TypeCode, value = Val};
	_->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
codec_decode_value(_Version, _Bytes, _TypeCode) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


%%------------------------------------------------------------
%%------------- SERVER SIDE FUNCTIONS ------------------------
%%------------------------------------------------------------
%% To make a long story short, you find an conceptual description
%% of how, and in which order, the different functions is 
%% supposed to be invoked.
%%
%%request_from_iiop(Bytes) -> 
%%    Reply = 
%%	case receive_service_contexts(ServerRequestInfo) of 
%%	    SYSTEM EXC -> 
%%		send_exception(..);
%%	    ForwardRequest EXC -> 
%%		send_other(..);
%%	    NoEXC -> 
%%		case receive_request(..) of
%%		    SYSTEM EXC -> 
%%			send_exception(..);
%%		    ForwardRequest EXC -> 
%%			send_other(..);
%%		    No EXC -> 
%%			InvokeServer
%%		end
%%	end,
%%    case Reply of
%%	EXC -> 
%%	    send_exception(..);
%%	No EXC, Normal Reply -> 
%%	    case send_reply(..) of
%%		SYSTEM EXC -> 
%%		    send_exception(..);
%%		ForwardRequest EXC -> 
%%		    send_other(..);
%%		No Exc -> 
%%		    Done
%%	    end;
%%	No EXC, LOCATION_FORWARD ->  
%%	    send_other(..) 
%%    end.
%% 
%% 
%%------------------------------------------------------------
%% function : server_start_receive
%% Arguments: Msg - #giop_message{}
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
server_start_receive(PIs, Version, ReqHdr, Rest, Len, ByteOrder, Msg) ->
    cdr_decode:dec_request_body(Version, ReqHdr, Rest, Len, ByteOrder, Msg),
    SRI = ?createInitSRI(ReqHdr#request_header.request_id,
			 ReqHdr#request_header.operation,
			 ReqHdr#request_header.response_expected),
    server_receive(receive_service_contexts, SRI, PIs, [], PIs).

server_receive(receive_service_contexts, SRI, [], _Acc, PIs) ->
    server_receive(receive_request, SRI, PIs, [], PIs);
server_receive(receive_service_contexts, SRI, [H|T], Acc, PIs) ->
    case catch receive_service_contexts(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=_Obj, 
							    permanent=_Bool}} ->
	    server_send(send_other, SRI, Acc, [], PIs);
	{'EXCEPTION', _E} ->
	    server_send(send_exception, SRI, Acc, [], PIs);
	_ ->
	    server_receive(receive_service_contexts, SRI, T, Acc, PIs)
    end;
server_receive(receive_request, SRI, [], _Acc, _PIs) ->
    %% Done with receive interceptors, now we can call the server.
    SRI;
server_receive(receive_request, SRI, [H|T], Acc, PIs)  ->
    case catch receive_request(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=_Obj, 
							    permanent=_Bool}} ->
	    server_send(send_other, SRI, Acc, [], PIs);
	{'EXCEPTION', _E} ->
	    server_send(send_exception, SRI, Acc, [], PIs);
	_ ->
	    server_receive(receive_request, SRI, T, Acc, PIs)
    end.


%%------------------------------------------------------------
%% function : server_start_send
%% Arguments: SRI - ServerRequestInfo
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
server_start_send(PIs, SRI) ->
    case SRI#'ServerRequestInfo'.reply_status of
	'PortableInterceptor_SUCCESSFUL' ->
	    server_send(send_reply, SRI, PIs, [], PIs);
	'PortableInterceptor_SYSTEM_EXCEPTION' ->
	    server_send(send_exception, SRI, PIs, [], PIs);
	'PortableInterceptor_USER_EXCEPTION' ->
	    server_send(send_exception, SRI, PIs, [], PIs);
	_ ->
	    server_send(send_other, SRI, PIs, [], PIs)
    end.

server_send(_, SRI, [], _Acc, _PIs) ->
    %% Done
    SRI;
server_send(send_exception, SRI, [H|T], Acc, PIs) ->
    case catch send_exception(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=_Obj, 
							    permanent=_Bool}} ->
	    server_send(send_other, SRI, Acc, [], PIs);
	{'EXCEPTION', _E} ->
	    server_send(send_exception, SRI, Acc, [], PIs);
	_ ->
	    server_send(send_exception, SRI, T, Acc, PIs)
    end;
server_send(send_other, SRI, [H|T], Acc, PIs) ->
    case catch send_other(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=_Obj, 
							    permanent=_Bool}} ->
	    server_send(send_other, SRI, T, Acc, PIs);
	{'EXCEPTION', _E} ->
	    server_send(send_exception, SRI, T, Acc, PIs);
	_ ->
	    server_send(send_other, SRI, T, Acc, PIs)
    end;
server_send(send_reply, SRI, [H|T], Acc, PIs) ->
    case catch send_reply(SRI, H) of
	{'EXCEPTION', _E} ->
	    server_send(send_exception, SRI, T, Acc, PIs);
	_ ->
	    server_send(send_reply, SRI, T, Acc, PIs)
    end.

receive_request(SRI, Mod) ->
    apply(Mod, receive_request, [SRI]).

send_other(SRI, Mod) ->
    apply(Mod, send_other, [SRI]).

receive_service_contexts(SRI, Mod) ->
    apply(Mod, receive_service_contexts, [SRI]).

send_reply(SRI, Mod) ->
    apply(Mod, send_reply, [SRI]).

send_exception(SRI, Mod) ->
    apply(Mod, send_exception, [SRI]).


%%------------------------------------------------------------
%%------------- CLIENT SIDE FUNCTIONS ------------------------
%%------------------------------------------------------------
%% To make a long story short, you find an conceptual description
%% of how, and in which order, the different functions is 
%% supposed to be invoked.
%%
%%request(Data) -> 
%%    Reply = 
%%	case send_request(CRI) of 
%%	    SYSTEM EXC -> 
%%		receive_exception(..);
%%	    ForwardRequest EXC -> 
%%		receive_other(..);
%%	    NoEXC -> 
%%		IIOP-send
%%	end,
%%    case Reply of
%%	EXC -> 
%%	    receive_exception(..); May raise system exc => receive_other(..);
%%	No EXC, Normal Reply -> 
%%	    receive_reply(..) May raise system exc => receive_exception(..);
%%	Non-normal reply (e.g. LOCATION_FORWARD) ->  
%%	    receive_other(..) May raise system exc => receive_exception(..);
%%    end.
%%------------------------------------------------------------
%% function : client_send
%% Arguments: CRI - ClientRequestInfo
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

client_send(CRI, PIs) ->
    client_send(send_request, CRI, PIs, [], PIs).

client_send(send_request, CRI, [], _, _) ->
    CRI;
client_send(send_request, CRI, [H|T], Acc, PIs) ->
    case catch send_request(CRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=_Obj, 
							    permanent=_Bool}} ->
	    client_receive(receive_other, CRI, T, [], PIs);
	{'EXCEPTION', _E} ->
	    client_receive(receive_exception, CRI, Acc, [], PIs);
	_ ->
	    client_send(send_request, CRI, T, Acc, PIs)
    end.
	


%%------------------------------------------------------------
%% function : client_receive
%% Arguments: CRI - ClientRequestInfo
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

client_receive(CRI, PIs) ->
    case CRI#'ClientRequestInfo'.reply_status of
	'PortableInterceptor_SUCCESSFUL' ->
	    client_receive(receive_reply, CRI, PIs, [], PIs);
	'PortableInterceptor_SYSTEM_EXCEPTION' ->
	    client_receive(receive_exception, CRI, PIs, [], PIs);
	'PortableInterceptor_USER_EXCEPTION' ->
	    client_receive(receive_exception, CRI, PIs, [], PIs);
	_ ->
	    client_receive(receive_other, CRI, PIs, [], PIs)
    end.

client_receive(_, CRI, [], _, _) ->
    %% Done
    CRI;
client_receive(receive_reply, CRI, [H|T], Acc, PIs) ->
    case catch receive_reply(CRI, H) of
	{'EXCEPTION', _E} ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs);
	_ ->
	    client_receive(receive_reply, CRI, T, [H|Acc], PIs)
    end;
client_receive(receive_exception, CRI, [H|T], Acc, PIs) ->
    case catch receive_exception(CRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=_Obj, 
							    permanent=_Bool}} ->
	    client_receive(receive_other, CRI, T, [], PIs);
	{'EXCEPTION', _E} ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs);
	_ ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs)
    end;
client_receive(receive_other, CRI, [H|T], Acc, PIs) ->
    case catch receive_other(CRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=_Obj, 
							    permanent=_Bool}} ->
	    client_receive(receive_other, CRI, T, [], PIs);
	{'EXCEPTION', _E} ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs);
	_ ->
	    client_receive(receive_other, CRI, T, [H|Acc], PIs)
    end.



send_request(CRI, Mod) ->
    apply(Mod, send_request, [CRI]).

receive_reply(CRI, Mod) ->
    apply(Mod, receive_reply, [CRI]).

receive_other(CRI, Mod) ->
    apply(Mod, receive_other, [CRI]).

receive_exception(CRI, Mod) ->
    apply(Mod, receive_exception, [CRI]).

%%------------------------------------------------------------
%% Functions for retrieving info from RequestInfo
%% ServerRequestInfo and ClientRequestInfo. The ones matching
%% both ServerRequestInfo and ClientRequestInfo eq. RequestInfo.
%% Note, RequestInfo is inherrited by the others.
%%------------------------------------------------------------
%%-----------------------------------------------------------%
%% function : _get_request_id
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : ulong()
%%------------------------------------------------------------
'_get_request_id'(#'ClientRequestInfo'{request_id = ID}) ->
    ID;
'_get_request_id'(#'ServerRequestInfo'{request_id = ID}) ->
    ID.

%%-----------------------------------------------------------%
%% function : _get_operation
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : string()
%%------------------------------------------------------------
'_get_operation'(#'ClientRequestInfo'{operation = Op}) ->
    Op;
'_get_operation'(#'ServerRequestInfo'{operation = Op}) ->
    Op.

%%-----------------------------------------------------------%
%% function : _get_arguments
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of #'Dynamic_Parameter'{}
%%------------------------------------------------------------
'_get_arguments'(#'ClientRequestInfo'{arguments = Args}) ->
    Args;
'_get_arguments'(#'ServerRequestInfo'{arguments = Args}) ->
    Args.

%%-----------------------------------------------------------%
%% function : _get_exceptions
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of CORBA::TypeCode
%%------------------------------------------------------------
'_get_exceptions'(#'ClientRequestInfo'{exceptions = Exc}) ->
    Exc;
'_get_exceptions'(#'ServerRequestInfo'{exceptions = Exc}) ->
    Exc.

%%-----------------------------------------------------------%
%% function : _get_contexts
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of CORBA::StringSeq
%%------------------------------------------------------------
'_get_contexts'(#'ClientRequestInfo'{contexts = Ctx}) ->
    Ctx;
'_get_contexts'(#'ServerRequestInfo'{contexts = Ctx}) ->
    Ctx.

%%-----------------------------------------------------------%
%% function : _get_operation_context
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of CORBA::StringSeq
%%------------------------------------------------------------
'_get_operation_context'(#'ClientRequestInfo'{operation_context = OpCtx}) ->
    OpCtx;
'_get_operation_context'(#'ServerRequestInfo'{operation_context = OpCtx}) ->
    OpCtx.

%%-----------------------------------------------------------%
%% function : _get_result
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : #any{}
%%------------------------------------------------------------
'_get_result'(#'ClientRequestInfo'{result = Res}) ->
    Res;
'_get_result'(#'ServerRequestInfo'{result = Res}) ->
    Res.

%%-----------------------------------------------------------%
%% function : _get_response_expected
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : boolean()
%%------------------------------------------------------------
'_get_response_expected'(#'ClientRequestInfo'{response_expected = Bool}) ->
    Bool;
'_get_response_expected'(#'ServerRequestInfo'{response_expected = Bool}) ->
    Bool.

%%-----------------------------------------------------------%
%% function : _get_sync_scope
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : Messaging::SyncScoope ('SYNC_NONE', 'SYNC_WITH_TRANSPORT', 
%%            'SYNC_WITH_SERVER', 'SYNC_WITH_TARGET')
%%------------------------------------------------------------
'_get_sync_scope'(#'ClientRequestInfo'{sync_scope = SS}) ->
    SS;
'_get_sync_scope'(#'ServerRequestInfo'{sync_scope = SS}) ->
    SS.

%%-----------------------------------------------------------%
%% function : _get_reply_status
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : ReplyStatus (short), defined in orber_pi.hrl
%%------------------------------------------------------------
'_get_reply_status'(#'ClientRequestInfo'{reply_status = RS}) ->
    RS;
'_get_reply_status'(#'ServerRequestInfo'{reply_status = RS}) ->
    RS.

%%-----------------------------------------------------------%
%% function : _get_forward_reference
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : Object
%%------------------------------------------------------------
'_get_forward_reference'(#'ClientRequestInfo'{forward_reference = FR}) ->
    FR;
'_get_forward_reference'(#'ServerRequestInfo'{forward_reference = FR}) ->
    FR.

%%------------------------------------------------------------
%% function : get_slot
%% Arguments: ClientRequestInfo or ServerRequestInfo
%%            SlotId - ulong()
%% Returns  : {'EXCEPTION', #'PortableInterceptor_InvalidSlot'{}}
%%------------------------------------------------------------
get_slot(_XRI, _SlotId) ->
    corba:raise(#'PortableInterceptor_InvalidSlot'{}).

%%------------------------------------------------------------
%% function : get_request_service_context
%% Arguments: ClientRequestInfo or ServerRequestInfo
%%            ServiceId - IOP::ServiceId (defined in orber_iiop.hrl)
%% Returns  : IOP::ServiceContext
%%------------------------------------------------------------
get_request_service_context(#'ClientRequestInfo'{contexts = Ctx}, _ServiceId) ->
    Ctx;
get_request_service_context(#'ServerRequestInfo'{contexts = Ctx}, _ServiceId) ->
    Ctx.

%%------------------------------------------------------------
%% function : get_reply_service_context
%% Arguments: ClientRequestInfo or ServerRequestInfo
%%            ServiceId - IOP::ServiceId (defined in orber_iiop.hrl)
%% Returns  :  IOP::ServiceContext
%%------------------------------------------------------------
get_reply_service_context(#'ClientRequestInfo'{contexts = Ctx}, _ServiceId) ->
    Ctx;
get_reply_service_context(#'ServerRequestInfo'{contexts = Ctx}, _ServiceId) ->
    Ctx.
    
%%------------------------------------------------------------
%%-------------- ClientRequestInfo only ----------------------
%%-----------------------------------------------------------%
%% function : _get_target
%% Arguments: ClientRequestInfo
%% Returns  : Object
%%------------------------------------------------------------
'_get_target'(#'ClientRequestInfo'{target = Target}) ->
    Target.

%%-----------------------------------------------------------%
%% function : _get_effective_target
%% Arguments: ClientRequestInfo
%% Returns  : Object
%%------------------------------------------------------------
'_get_effective_target'(#'ClientRequestInfo'{effective_target = ET}) ->
    ET.

%%-----------------------------------------------------------%
%% function : _get_effective_profile
%% Arguments: ClientRequestInfo
%% Returns  : IOP:TaggedProfile
%%------------------------------------------------------------
'_get_effective_profile'(#'ClientRequestInfo'{effective_profile = EP}) ->
    EP.

%%-----------------------------------------------------------%
%% function : _get_received_exception
%% Arguments: ClientRequestInfo
%% Returns  : #any{}
%%------------------------------------------------------------
'_get_received_exception'(#'ClientRequestInfo'{received_exception = RE}) ->
    RE.

%%-----------------------------------------------------------%
%% function : _get_received_exception
%% Arguments: ClientRequestInfo
%% Returns  : CORBA::RepositoryId
%%------------------------------------------------------------
'_get_received_exception_id'(#'ClientRequestInfo'{received_exception_id = REId}) ->
    REId.

%%------------------------------------------------------------
%% function : get_effective_component
%% Arguments: ClientRequestInfo
%% Returns  : IOR::TaggedComponent
%%------------------------------------------------------------
get_effective_component(#'ClientRequestInfo'{target = Target}, _Id) ->
    Target.

%%------------------------------------------------------------
%% function : get_effective_components
%% Arguments: ClientRequestInfo
%%            Id -IOP::ComponentId (ulong())
%% Returns  : IOP_N::TaggedComponentSeq
%%------------------------------------------------------------
get_effective_components(#'ClientRequestInfo'{target = Target}, _Id) ->
    Target.

%%------------------------------------------------------------
%% function : get_request_policy
%% Arguments: ClientRequestInfo
%%            Type - CORBA::PolicyType
%% Returns  : IOP_N::TaggedComponentSeq
%%------------------------------------------------------------
get_request_policy(#'ClientRequestInfo'{target = Target}, _Type) ->
    Target.

%%------------------------------------------------------------
%% function : add_request_service_context
%% Arguments: ClientRequestInfo
%%            Ctx - IOP::ServiceContext
%%            Replace - boolean()
%% Returns  : -
%%------------------------------------------------------------
add_request_service_policy(#'ClientRequestInfo'{target = _Target}, 
			   _Ctx, _Replace) ->
    ok.

%%------------------------------------------------------------
%%-------------- ServerRequestInfo only ----------------------
%%-----------------------------------------------------------%
%% function : _get_sending_exception
%% Arguments: ServerRequestInfo
%% Returns  : #any{}
%%------------------------------------------------------------
'_get_sending_exception'(#'ServerRequestInfo'{sending_exception = Exc}) ->
    Exc.

%%-----------------------------------------------------------%
%% function : _get_object_id
%% Arguments: ServerRequestInfo
%% Returns  : CORBA::OctetSeq
%%------------------------------------------------------------
'_get_object_id'(#'ServerRequestInfo'{object_id = OI}) ->
    OI.

%%-----------------------------------------------------------%
%% function : _get_adapter_id
%% Arguments: ServerRequestInfo
%% Returns  : CORBA::OctetSeq
%%------------------------------------------------------------
'_get_adapter_id'(#'ServerRequestInfo'{adapter_id = AI}) ->
    AI.

%%-----------------------------------------------------------%
%% function : _get_target_most_derived_interface
%% Arguments: ServerRequestInfo
%% Returns  : CORBA::RepositoryId
%%------------------------------------------------------------
'_get_target_most_derived_interface'(#'ServerRequestInfo'
				     {target_most_derived_interface = TMDI}) ->
    TMDI.

%%------------------------------------------------------------
%% function : get_server_policy
%% Arguments: ServerRequestInfo
%%            PolicyType - CORBA::PolicyType
%% Returns  : CORBA::Policy
%%------------------------------------------------------------
get_server_policy(#'ServerRequestInfo'{contexts = Ctxs}, _PolicyType) ->
    Ctxs.

%%------------------------------------------------------------
%% function : set_slot
%% Arguments: ServerRequestInfo
%%            SlotId - ulong()
%%            Data - #any{}
%% Returns  : {'EXCEPTION', #'PortableInterceptor_InvalidSlot'{}}
%%------------------------------------------------------------
set_slot(_SRI, _SlotId, _Data) ->
    corba:raise(#'PortableInterceptor_InvalidSlot'{}).

%%-----------------------------------------------------------%
%% function : target_is_a
%% Arguments: ServerRequestInfo
%%            IFRId - CORBA::RepositoryId
%% Returns  : boolean()
%%------------------------------------------------------------
target_is_a(#'ServerRequestInfo'{object_id = ObjId}, IFRId) ->
    corba_object:is_a(ObjId, IFRId).

%%------------------------------------------------------------
%% function : add_reply_service_context
%% Arguments: ServerRequestInfo
%%            Ctx - IOP::ServiceContext
%%            Replace - boolean()
%% Returns  : -
%%------------------------------------------------------------
add_reply_service_context(#'ServerRequestInfo'{contexts = Ctxs}, _Ctx, _Replace) ->
    Ctxs.


%%--------------- END OF MODULE ------------------------------
