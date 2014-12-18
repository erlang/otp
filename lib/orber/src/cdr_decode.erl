%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: cdr_decode.erl
%% 
%% Description:
%%    This file contains all decoding functions for the CDR
%%    format.
%%
%%-----------------------------------------------------------------
-module(cdr_decode).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/corba.hrl").

-include_lib("orber/src/ifr_objects.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([dec_giop_message_header/1, dec_reply_header/4,
	 dec_reply_body/6, dec_locate_reply_header/4, 
	 dec_locate_reply_body/5, dec_message_header/3, dec_request_body/6,
	 dec_octet_sequence_bin/6, dec_message/2, peek_request_id/2]).

%%-----------------------------------------------------------------
%% Functions which only are exported for the testcases.
%%-----------------------------------------------------------------
-export([dec_type/5, dec_byte_order/1, dec_system_exception/4, dec_user_exception/4,
	 dec_byte_order_list/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 9).

-define(ODD(N), (N rem 2) == 1).

%%-----------------------------------------------------------------
%% Func: dec_message/3
%% Args: 
%%       TypeCodes - is the type_codes of the return value and out parameters
%%                   when one decodes a reply.
%%       Bytes - is the the message as a byte sequence.
%% Returns: 
%%       A tupple which contains the decoded message,
%%       {ok, Header, Parameters, TypeCodes}.
%%-----------------------------------------------------------------
dec_message(TypeCodes, Bytes) ->
    Message = dec_giop_message_header(Bytes),
    case Message#giop_message.message_type of
        ?GIOP_MSG_REQUEST ->
            {Version, ReqHdr, Rest, Len, ByteOrder} = 
		dec_request_header(Message#giop_message.giop_version,
				   Message#giop_message.message, ?GIOP_HEADER_SIZE, 
				   Message#giop_message.byte_order, Bytes),
	    dec_request_body(Version, ReqHdr, Rest, Len, ByteOrder, Bytes);
        ?GIOP_MSG_REPLY ->
            dec_reply(Message#giop_message.giop_version,
                      TypeCodes, Message#giop_message.message, ?GIOP_HEADER_SIZE,
                      Message#giop_message.byte_order);
        ?GIOP_MSG_CANCEL_REQUEST ->
            dec_cancel_request(Message#giop_message.giop_version,
                               Message#giop_message.message, ?GIOP_HEADER_SIZE, 
                               Message#giop_message.byte_order);
        ?GIOP_MSG_LOCATE_REQUEST ->
            dec_locate_request(Message#giop_message.giop_version,
                               Message#giop_message.message, ?GIOP_HEADER_SIZE, 
                               Message#giop_message.byte_order);
        ?GIOP_MSG_LOCATE_REPLY ->
            dec_locate_reply(Message#giop_message.giop_version,
                             Message#giop_message.message, ?GIOP_HEADER_SIZE, 
                             Message#giop_message.byte_order);
        ?GIOP_MSG_CLOSE_CONNECTION ->
            'close_connection';
        ?GIOP_MSG_MESSAGE_ERROR ->
            'message_error';
        ?GIOP_MSG_FRAGMENT ->
	    dec_fragment_header(Message#giop_message.giop_version,
				Message#giop_message.message, ?GIOP_HEADER_SIZE, 
				Message#giop_message.byte_order, Bytes)
    end.

%%-----------------------------------------------------------------
%% Func: dec_giop_message_header/1
%% Args: 
%%       Bytes - is the the message as a byte sequence.
%% Returns: 
%%       A giop_message record.
%%-----------------------------------------------------------------
%%                         Magic|Version|BO| Type | Size | Body
dec_giop_message_header(<<"GIOP",1:8,0:8,1:8,MessType:8,
			MessSize:32/little-unsigned-integer,Message/binary>>) ->
    #giop_message{magic = "GIOP", giop_version = {1,0},
		  byte_order = little, message_type = MessType, 
		  message_size = MessSize, message = Message};
dec_giop_message_header(<<"GIOP",1:8,0:8,0:8,MessType:8,
			MessSize:32/big-unsigned-integer,Message/binary>>) ->
    #giop_message{magic = "GIOP", giop_version = {1,0},
		  byte_order = big, message_type = MessType, 
		  message_size = MessSize, message = Message};
dec_giop_message_header(<<"GIOP",1:8,Minor:8,Flags:8,MessType:8,
			MessSize:32/little-unsigned-integer,Message/binary>>) when
  ((Flags band 16#01) == 16#01) ->
    #giop_message{magic = "GIOP", giop_version = {1,Minor},
		  byte_order = little, fragments = ((Flags band 16#02) == 16#02),
		  message_type = MessType, message_size = MessSize, message = Message};
dec_giop_message_header(<<"GIOP",1:8,Minor:8,Flags:8,MessType:8,
			MessSize:32/big-unsigned-integer,Message/binary>>) ->
    #giop_message{magic = "GIOP", giop_version = {1,Minor},
		  byte_order = big, fragments = ((Flags band 16#02) == 16#02),
		  message_type = MessType, message_size = MessSize, message = Message};
dec_giop_message_header(<<Hdr:?GIOP_HEADER_SIZE/binary, _Body/binary>>) ->
    orber:dbg("[~p] cdr_decode:dec_giop_message_header(~p);~n"
	      "Orber cannot decode the GIOP-header.", [?LINE, Hdr], ?DEBUG_LEVEL),
    exit(message_error);
dec_giop_message_header(Other) ->
    orber:dbg("[~p] cdr_decode:dec_giop_message_header(~p);~n"
	      "Orber cannot decode the GIOP-header.", [?LINE, Other], ?DEBUG_LEVEL),
    exit(message_error).


peek_request_id(big, <<ReqId:32/big-unsigned-integer,_/binary>>) ->
    ReqId;
peek_request_id(little, <<ReqId:32/little-unsigned-integer,_/binary>>) ->
    ReqId.

%%-----------------------------------------------------------------
%% Func: dec_message_header/2
%% Args: 
%%       Header - #giop_message{}
%%       Bytes - is the the message body as a byte sequence.
%% Returns: 
%%-----------------------------------------------------------------
dec_message_header(TypeCodes, Message, Bytes) ->
    case Message#giop_message.message_type of
	?GIOP_MSG_REQUEST ->
	    dec_request_header(Message#giop_message.giop_version,
			       Message#giop_message.message, ?GIOP_HEADER_SIZE, 
			       Message#giop_message.byte_order, Bytes);
	?GIOP_MSG_REPLY ->
	    dec_reply(Message#giop_message.giop_version,
		      TypeCodes, Message#giop_message.message, ?GIOP_HEADER_SIZE,
		      Message#giop_message.byte_order);
	?GIOP_MSG_CANCEL_REQUEST ->
	    dec_cancel_request(Message#giop_message.giop_version,
			       Message#giop_message.message, ?GIOP_HEADER_SIZE, 
			       Message#giop_message.byte_order);
	?GIOP_MSG_LOCATE_REQUEST ->
	    dec_locate_request(Message#giop_message.giop_version,
			       Message#giop_message.message, ?GIOP_HEADER_SIZE, 
			       Message#giop_message.byte_order);
	?GIOP_MSG_LOCATE_REPLY ->
	    dec_locate_reply(Message#giop_message.giop_version,
			     Message#giop_message.message, ?GIOP_HEADER_SIZE, 
			     Message#giop_message.byte_order);
	?GIOP_MSG_CLOSE_CONNECTION ->
	    'close_connection';
	?GIOP_MSG_MESSAGE_ERROR ->
	    'message_error';
	?GIOP_MSG_FRAGMENT ->
	    dec_fragment_header(Message#giop_message.giop_version,
				Message#giop_message.message, ?GIOP_HEADER_SIZE, 
				Message#giop_message.byte_order, Bytes)	    
    end.


%%-----------------------------------------------------------------
%% Func: dec_byte_order/1
%% Args: 
%%       The message as a byte sequence.
%% Returns: 
%%       A tuple {Endianness, Rest} where Endianness is big or little.
%%       Rest is the remaining message byte sequence.
%%-----------------------------------------------------------------
dec_byte_order(<<0:8,T/binary>>) ->
    {big, T};
dec_byte_order(<<1:8,T/binary>>) ->
    {little, T}.

%%-----------------------------------------------------------------
%% Func: dec_byte_order_list/1
%% Args: 
%%       The message as a byte sequence.
%% Returns: 
%%       A tuple {Endianness, Rest} where Endianness is big or little.
%%       Rest is the remaining message byte sequence.
%%-----------------------------------------------------------------
dec_byte_order_list([0|T]) ->
    {big, T};
dec_byte_order_list([1|T]) ->
    {little, T}.

%%-----------------------------------------------------------------
%% Func    : dec_response_flags
%% Args    : 
%% Returns : boolean
%%-----------------------------------------------------------------
%% FIX ME!! Not correct flag handling.
dec_response_flags(_Version, <<0:8, Rest/binary>>, Len) ->
    {false, Rest, Len+1};
dec_response_flags(_Version, <<1:8, Rest/binary>>, Len) ->
    {true_oneway, Rest, Len+1};
dec_response_flags(_Version, <<3:8, Rest/binary>>, Len) ->
    {true, Rest, Len+1};
dec_response_flags(_Version, <<X:8, Rest/binary>>, Len) ->
    %% Not only the Response flag is set, test which.
    if
	%% Since the 6 most significant bits are unused we'll accept this for now.
	((X band 16#03) == 16#03) ->
	    {true, Rest, Len+1};
	((X band 16#01) == 16#01) ->
	    {true_oneway, Rest, Len+1};
	true ->
	    {false, Rest, Len+1}
    end.

%%-----------------------------------------------------------------
%% Func    : dec_target_addr
%% Args    : Octet
%% Returns : boolean
%%-----------------------------------------------------------------
dec_target_addr(Version, Message, Len, ByteOrder, RequestId, Type) ->
    case dec_type(?TARGETADDRESS, Version, Message, Len, ByteOrder, [], 0) of
	{#'GIOP_TargetAddress'{label = ?GIOP_KeyAddr, value = KeyAddr}, Rest3, Len3, C} ->
	    {dec_target_key(KeyAddr, RequestId, Version, Type), Rest3, Len3, C};
	{#'GIOP_TargetAddress'{label = ?GIOP_ProfileAddr, 
			       value = #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
							    profile_data=PA}}, 
	 Rest3, Len3, C} ->
	    {dec_target_key(PA, RequestId, Version, Type), Rest3, Len3, C};
	{#'GIOP_TargetAddress'{label = ?GIOP_ReferenceAddr,
			       value = #'GIOP_IORAddressingInfo'{
				 selected_profile_index = _PI,
				 ior = IOR}}, Rest3, Len3, C} ->
	    {dec_target_key(iop_ior:get_objkey(IOR), RequestId, Version, Type), 
	     Rest3, Len3, C};
	Other ->
	    orber:dbg("[~p] cdr_decode:dec_target_addr(~p);~n"
		      "Unsupported TargetAddress.", [?LINE, Other], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 12), completion_status=?COMPLETED_MAYBE})
    end.

%%-----------------------------------------------------------------
%% Func    : dec_target_key
%% Args    : Octet
%% Returns : boolean
%%-----------------------------------------------------------------
dec_target_key(Key, RequestId, Version, Type) ->
    %% The Type argument is used as an identifier of which operation it is.
    %% We need it to be able to tell the difference if it's, for example,
    %% a request or locate-request.
    case corba:string_to_objkey_local(Key) of
	{location_forward, Object} ->
	    throw({Type, Object, RequestId, Version, Key});
	ObjRef ->
	    ObjRef
    end.

%%-----------------------------------------------------------------
%% Func: dec_request_header/3
%% Args: 
%%       Message - The message
%%       Len0 - Number of bytes already read.
%%       ByteOrder - little or big
%% Returns: 
%%-----------------------------------------------------------------
dec_request_header(Version, Message, Len0, ByteOrder, _Buffer) when Version == {1,2} ->
    {Request_id, Rest1, Len1, _} = dec_type('tk_ulong', Version, Message, Len0, 
					    ByteOrder, [], 0),
    {ResponseFlags, Rest2, Len2} = dec_response_flags(Version, Rest1, Len1),
    {_, Rest2b, Len2b, _} = dec_type({'tk_array', 'tk_octet', 3}, Version, Rest2, Len2, ByteOrder, [], 0),
    {Object_key, Rest3, Len3, _} = dec_target_addr(Version, Rest2b, Len2b, ByteOrder, Request_id,
						   'location_forward'),
    {Operation, Rest4, Len4, _} = dec_type({'tk_string', 0},  Version, Rest3, Len3, ByteOrder, [], 0),
    {Context, Rest5, Len5} = dec_service_context(Version, Rest4, Len4, ByteOrder),
    {Version, #request_header{service_context=Context, 
			      request_id=Request_id,
			      response_expected=ResponseFlags,
			      object_key=Object_key,
			      operation=list_to_atom(Operation), 
			      requesting_principal=""}, Rest5, Len5, ByteOrder};
dec_request_header(Version, Message, Len0, ByteOrder, _Buffer) ->
    {Context, Rest1, Len1} = dec_service_context(Version, Message, Len0, ByteOrder),
    {Request_id, Rest2, Len2, _} = dec_type('tk_ulong', Version, Rest1, Len1, ByteOrder, [], 0),
    {Response_expected, Rest3, Len3, _} = dec_type('tk_boolean',  Version, Rest2, Len2,
						   ByteOrder, [], 0),
    {ObjKey, Rest4, Len4, _} = dec_type({'tk_sequence', 'tk_octet', 0},  Version, Rest3,
				   Len3, ByteOrder, [], 0),
    Object_key = dec_target_key(ObjKey, Request_id, Version, 'location_forward'),
    {Operation, Rest5, Len5, _} = dec_type({'tk_string', 0},  Version, Rest4, Len4, ByteOrder, [], 0),
    {Principal, Rest, Len, _} = dec_type({'tk_string', 0},  Version, Rest5,Len5,  ByteOrder, [], 0),
    {Version, #request_header{service_context=Context, 
			      request_id=Request_id,
			      response_expected=Response_expected,
			      object_key=Object_key,
			      operation=list_to_atom(Operation), 
			      requesting_principal=Principal}, Rest, Len, ByteOrder}.


%%-----------------------------------------------------------------
%% Func: dec_service_context/4
%% Args: Version - e.g. 1.2
%%       Message - The message
%%       Len - Number of bytes already read.
%%       ByteOrder - little or big
%% Returns: 
%%-----------------------------------------------------------------
dec_service_context(Version, Message, Len, ByteOrder) ->
    {Context, Rest, Len1} = dec_type(?IOP_SERVICECONTEXT, Version, Message, 
				     Len, ByteOrder),
    {dec_used_contexts(Version, Context, []), Rest, Len1}.

dec_used_contexts(_Version, [], Ctxs) ->
    Ctxs;
dec_used_contexts({1,0}, [#'IOP_ServiceContext'{context_id=?IOP_CodeSets}|T], Ctxs) ->
    %% Not supported by 1.0, drop it.
    dec_used_contexts({1,0}, T, Ctxs);
dec_used_contexts(Version, [#'IOP_ServiceContext'{context_id=?IOP_CodeSets,
						  context_data = Bytes}|T], Ctxs) ->
    {ByteOrder, Rest} = dec_byte_order(list_to_binary(Bytes)),
    {CodeCtx, _, _} =  dec_type(?CONV_FRAME_CODESETCONTEXT, Version, 
				Rest, 1, ByteOrder),
    dec_used_contexts(Version, T, 
		      [#'IOP_ServiceContext'{context_id=?IOP_CodeSets,
					     context_data = CodeCtx}|Ctxs]);
dec_used_contexts(Version, [#'IOP_ServiceContext'{context_id=?IOP_BI_DIR_IIOP,
						  context_data = Bytes}|T], Ctxs) ->
    {ByteOrder, Rest} = dec_byte_order(list_to_binary(Bytes)),
    {BiDirCtx, _, _} =  dec_type(?IIOP_BIDIRIIOPSERVICECONTEXT, Version, 
				 Rest, 1, ByteOrder),
    dec_used_contexts(Version, T, 
		      [#'IOP_ServiceContext'{context_id=?IOP_BI_DIR_IIOP,
					     context_data = BiDirCtx}|Ctxs]);
dec_used_contexts(Version, [#'IOP_ServiceContext'{context_id=?IOP_FT_REQUEST,
						  context_data = Bytes}|T], Ctxs) ->
    {ByteOrder, Rest} = dec_byte_order(list_to_binary(Bytes)),
    {Ctx, _, _} =  dec_type(?FT_FTRequestServiceContext, Version, 
			    Rest, 1, ByteOrder),
    dec_used_contexts(Version, T, 
		      [#'IOP_ServiceContext'{context_id=?IOP_FT_REQUEST,
					     context_data = Ctx}|Ctxs]);
dec_used_contexts(Version, [#'IOP_ServiceContext'{context_id=?IOP_FT_GROUP_VERSION,
						  context_data = Bytes}|T], Ctxs) ->
    {ByteOrder, Rest} = dec_byte_order(list_to_binary(Bytes)),
    {Ctx, _, _} =  dec_type(?FT_FTGroupVersionServiceContext, Version, 
			    Rest, 1, ByteOrder),
    dec_used_contexts(Version, T, 
		      [#'IOP_ServiceContext'{context_id=?IOP_FT_GROUP_VERSION,
					     context_data = Ctx}|Ctxs]);
dec_used_contexts(Version, [#'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
						  context_data = Bytes}|T], Ctxs) ->
    {ByteOrder, Rest} = dec_byte_order(list_to_binary(Bytes)),
    {Ctx, _, _} =  dec_type(?CSI_SASContextBody, Version, 
			    Rest, 1, ByteOrder),
    dec_used_contexts(Version, T, 
		      [#'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
					     context_data = Ctx}|Ctxs]);
dec_used_contexts(Version, [#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
						  context_data = Bytes}|T], Ctxs) ->
    {ByteOrder, Rest} = dec_byte_order(list_to_binary(Bytes)),
    {Ctx, _, _} =  dec_type(?ORBER_GENERIC_CTX, Version, 
			    Rest, 1, ByteOrder),
    dec_used_contexts(Version, T, 
		      [#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					     context_data = binary_to_term(list_to_binary(Ctx))}|Ctxs]);
dec_used_contexts(Version, [H|T], Ctxs) ->
    dec_used_contexts(Version, T, [H|Ctxs]).

%%-----------------------------------------------------------------
%% Func: dec_request_body
%% Args: Version - e.g. 1.2
%% Returns: 
%%-----------------------------------------------------------------
dec_request_body(Version, ReqHdr, Rest, Len, ByteOrder, Buffer) ->
    {Parameters, TypeCodes, _} = 
	dec_request_body(Version, ReqHdr#request_header.object_key, 
			 ReqHdr#request_header.operation, 
			 Rest, Len, ByteOrder, Buffer, Len),
    {Version, ReqHdr, Parameters, TypeCodes}.

dec_request_body(Version, Object_key, Operation, Body, Len, ByteOrder, Buffer, Counter) 
  when Version == {1,2} ->
    case orber_typedefs:get_op_def(Object_key, Operation) of
	{RetType, [], OutParameters} ->
	    {[], {RetType, [], OutParameters}, Len};
	{RetType, InParameters, OutParameters} ->
	    {Rest, Len1, NewC} = dec_align(Body, Len, 8, Counter),
	    {Parameters, Len2} = dec_parameters(Version, InParameters, Rest, Len1, 
						ByteOrder, Buffer, NewC),
	    {Parameters, {RetType, InParameters, OutParameters}, Len2}
    end;
dec_request_body(Version, Object_key, Operation, Body, Len, ByteOrder, Buffer, Counter) ->
    {RetType, InParameters, OutParameters} =
	orber_typedefs:get_op_def(Object_key, Operation),
    {Parameters, Len1} = dec_parameters(Version, InParameters, Body, Len, ByteOrder, Buffer, Counter),
    {Parameters, {RetType, InParameters, OutParameters}, Len1}.

dec_parameters(_, [], _, Len, _, _, _) ->
    {[], Len};
dec_parameters(Version, [P1 |InParList], Body, Len, ByteOrder, Buffer, Counter) ->
    {Object, Rest, Len1, NewCounter} = dec_type(P1, Version, Body, Len, ByteOrder, Buffer, Counter),
    {List, Len2} = dec_parameters(Version, InParList, Rest, Len1, ByteOrder, Buffer, NewCounter),
    {[Object | List], Len2}.

%%-----------------------------------------------------------------
%% Func: dec_reply/5
%% Args: 
%%       Message - The message
%%       Len0 - Number of bytes already read.
%%       ByteOrder - little or big
%% Returns: 
%%       A tuple {ReplyHeader, Result} where ReplyHeader is a
%%       reply_header record and Result the decode result.
%%-----------------------------------------------------------------
dec_reply(Version, TypeCodes, Message, Len0, ByteOrder) ->
    {ReplyHeader, Rest, Len} = dec_reply_header(Version, Message, Len0, ByteOrder),
    {Result, Par} = 
	case ReplyHeader#reply_header.reply_status of
	    'no_exception' ->
		{R, P, _} = dec_reply_body(Version, TypeCodes, Rest, Len, ByteOrder, Message),
		{R, P};
	    'system_exception' ->
		{R, _} = dec_system_exception(Version, Rest, Len, ByteOrder),
		{R, []};
	    'user_exception' ->
		{R, _} = dec_user_exception(Version, Rest, Len, ByteOrder),
		{R, []};
	    'location_forward' ->
		{R, _, _} = dec_reply_body(Version, {{'tk_objref', "", ""}, [],[]}, 
					   Rest, Len, ByteOrder, Message),
	    {R, []};
	    %% This is deprecated in later version than CORBA-2.3.1. We'll leave it for
	    %% now.
	    'location_forward_perm' ->
		{R, _, _} = dec_reply_body(Version, {{'tk_objref', "", ""}, [],[]}, 
					   Rest, Len, ByteOrder, Message),
		{R, []};
	    'needs_addressing_mode' ->
		{R, _, _} = dec_reply_body(Version, {'tk_short', [],[]}, 
					   Rest, Len, ByteOrder, Message),
		{R, []}
	     end,
    {ReplyHeader, Result, Par}.


%% ## NEW IIOP 1.2 ##
dec_reply_header(Version, Message, Len0, ByteOrder) when Version == {1,2} ->
    {Request_id, Rest1, Len1} = dec_type('tk_ulong', Version, Message, Len0, ByteOrder),
    {ReplyStatus, Rest2, Len2} = dec_reply_status(Version, Rest1, Len1, ByteOrder),
    {Context, Rest, Len3} = dec_service_context(Version, Rest2, Len2, ByteOrder),
    {#reply_header{service_context=Context, request_id=Request_id, reply_status=ReplyStatus},
     Rest, Len3};

dec_reply_header(Version, Message, Len0, ByteOrder) ->
    {Context, Rest1, Len1} = dec_service_context(Version, Message, Len0, ByteOrder),
    {Request_id, Rest2, Len2} = dec_type('tk_ulong', Version, Rest1, Len1, ByteOrder),
    {ReplyStatus, Rest, Len3} = dec_reply_status(Version, Rest2, Len2, ByteOrder),
    {#reply_header{service_context=Context, request_id=Request_id, reply_status=ReplyStatus},
     Rest, Len3}.

dec_reply_status(Version, Status, Len, ByteOrder) ->
    {L, Rest, Len1}= dec_type('tk_ulong', Version, Status, Len, ByteOrder),
    {dec_giop_reply_status_type(L), Rest, Len1}.

dec_reply_body(_, {'tk_void', _, []}, <<>>, Len, _, _) ->
    %% This case is mainly to be able to avoid removing non-existent alignment for
    %% IIOP-1.2 messages if the body should be empty, i.e., void return value and
    %% no out parameters.
    {ok, [], Len};
dec_reply_body(Version, {RetType, _InParameters, OutParameters}, Body, Len, 
	       ByteOrder, Bytes) when Version == {1,2} ->
    {Rest, Len1, Counter} = dec_align(Body, Len, 8, Len),
    {Result, Rest2, Len2, C} = dec_type(RetType, Version, Rest, Len1, ByteOrder, Bytes, Counter),
    {Par, Len3} = dec_parameters(Version, OutParameters, Rest2, Len2, ByteOrder, Bytes, C),
    {Result, Par, Len3};
dec_reply_body(Version, {RetType, _InParameters, OutParameters}, Body, Len, ByteOrder, Bytes) ->
    {Result, Rest, Len1, C} = dec_type(RetType, Version, Body, Len, ByteOrder, Bytes, Len),
    {Par, Len2} = dec_parameters(Version, OutParameters, Rest, Len1, ByteOrder, Bytes, C),
    {Result, Par, Len2}.


%%-----------------------------------------------------------------
%% Func: dec_cancel_request/3
%% Args: 
%%       Message - The message
%%       Len - Number of bytes already read.
%%       ByteOrder - little or big
%% Returns: 
%%       A cancel_request_header record.
%%-----------------------------------------------------------------
dec_cancel_request(Version, Message, Len, ByteOrder) ->
    {Request_id, _, _} = dec_type('tk_ulong', Version, Message, Len, ByteOrder),
    #cancel_request_header{request_id=Request_id}.

%%-----------------------------------------------------------------
%% Func: dec_locate_request/3
%% Args: 
%%       Message - The message
%%       Len - Number of bytes already read.
%%       ByteOrder - little or big
%% Returns: 
%%       A locate_request_header record.
%%-----------------------------------------------------------------
%% ## NEW IIOP 1.2 ##
dec_locate_request(Version, Message, Len, ByteOrder) when Version == {1,2} ->
    {Request_id, Rest, Len1} = dec_type('tk_ulong', Version, Message, Len, ByteOrder),
    {Object_key, _, _, _} = dec_target_addr(Version, Rest, Len1, ByteOrder, Request_id, 
					    'object_forward'),
    {Version, #locate_request_header{request_id=Request_id, object_key=Object_key}};
dec_locate_request(Version, Message, Len, ByteOrder) ->
    {Request_id, Rest, Len1} = dec_type('tk_ulong', Version, Message, Len, ByteOrder),
    {ObjKey, _, _} = dec_type({'tk_sequence', 'tk_octet', 0}, Version, Rest,
				   Len1, ByteOrder),
    Object_key = dec_target_key(ObjKey, Request_id, Version, 'object_forward'),    
    {Version, #locate_request_header{request_id=Request_id, object_key=Object_key}}.


%%-----------------------------------------------------------------
%% Func: dec_locate_reply/3
%% Args: 
%%       Message - The message
%%       Len - Number of bytes already read.
%%       ByteOrder - little or big
%% Returns: 
%%       A locate_reply_header record.
%%-----------------------------------------------------------------
dec_locate_reply(Version, Message, Len, ByteOrder) ->
    {ReplyHeader, Rest1, Len1} = dec_locate_reply_header(Version, Message, Len, ByteOrder),
    {ReplyHeader, dec_locate_reply_body(Version, ReplyHeader#locate_reply_header.locate_status, Rest1, 
					Len1, ByteOrder)}.

dec_locate_reply_header(Version, Message, Len, ByteOrder) ->
    {Request_id, Rest1, Len1} = dec_type('tk_ulong', Version, Message, Len, ByteOrder),
    {Locate_status, Rest2, Len2} = dec_locate_status(Version, Rest1, Len1, ByteOrder),
    {#locate_reply_header{request_id=Request_id, locate_status=Locate_status}, Rest2, Len2}.
   
dec_locate_reply_body(Version, LocateStatus, Rest, Len, ByteOrder) when Version == {1,2} ->
    %% In CORBA-2.3.1 the LocateReply body didn't align the body (8-octet
    %% boundry) for IIOP-1.2. This have been changed in CORBA-2.4 and
    %% changed back in CORBA-2.6. Hence, we should not change this.
    case LocateStatus of
	'object_forward' ->
	    {ObjRef, _, _, _} = dec_objref(Version, Rest, Len, ByteOrder),
	    ObjRef;
	'object_forward_perm' ->
	    %% This is deprecated in later version than CORBA-2.3.1. We'll leave it for
	    %% now.
	    {ObjRef, _, _, _} = dec_objref(Version, Rest, Len, ByteOrder),
	    ObjRef;
	'loc_system_exception' ->
	    %% This should be updated but since 'dec_system_exception' removes
	    %% alignment, which the LocateReplyBody don't have, for 1.2 we
	    %% pretend it's 1.1 for now. 
	    {SysExc, _} = dec_system_exception({1,1}, Rest, Len, ByteOrder),
	    corba:raise(SysExc);
	'loc_needs_addressing_mode' ->
	    %% Not supported.
	    [];
	_ ->
	    []
    end;
dec_locate_reply_body(Version, LocateStatus, Rest, Len, ByteOrder) ->
    case LocateStatus of
	'object_forward' ->
	    {ObjRef, _, _, _} = dec_objref(Version, Rest, Len, ByteOrder),
	    ObjRef;
	_ ->
	    []
    end.

dec_locate_status(Version, Bytes, Len, ByteOrder) ->
    {L, Rest, Len1} = dec_type('tk_ulong', Version, Bytes, Len, ByteOrder),
    {dec_giop_locate_status_type(L), Rest, Len1}.


%%-----------------------------------------------------------------
%% Func: dec_fragment_header/5
%% Args: 
%%       Message - The message
%%       Len0 - Number of bytes already read.
%%       ByteOrder - little or big
%% Returns: 
%%-----------------------------------------------------------------
dec_fragment_header(Version, Message, Len0, ByteOrder, _Buffer) when Version == {1,2} ->
    {RequestId, Rest1, Len1, _} = dec_type('tk_ulong', Version, Message, Len0, 
					    ByteOrder, [], 0),
    {Version, #fragment_header{request_id=RequestId}, Rest1, Len1, ByteOrder};
dec_fragment_header(Version, _Message, _Len0, _ByteOrder, _Buffer) ->
    %% The FragmentHeader is IIOP-1.2 specific. Hence, do nothing here.
    orber:dbg("[~p] cdr_decode:dec_fragment_header(~p)~n"
	      "Orber only supports fragmented messages for IIOP-1.2.", 
	      [?LINE, Version], ?DEBUG_LEVEL),
    exit(message_error).
%    {Version, #fragment_header{}, Message, Len0, ByteOrder}.

%%-----------------------------------------------------------------
%% Func: dec_giop_reply_status_type
%% Args:
%%       An integer status code
%% Returns:
%%       An atom which is the reply status
%%-----------------------------------------------------------------
dec_giop_reply_status_type(0) ->
    'no_exception';
dec_giop_reply_status_type(1) ->
    'user_exception';
dec_giop_reply_status_type(2) ->
    'system_exception';
dec_giop_reply_status_type(3) ->
    'location_forward';
%% ## IIOP-1.2 ##
dec_giop_reply_status_type(4) ->
    'location_forward_perm';
dec_giop_reply_status_type(5) ->
    'needs_addressing_mode'.
 
%%-----------------------------------------------------------------
%% Func: dec_giop_locate_status_type
%% Args:
%%       An integer status code
%% Returns:
%%       An atom which is the reply status
%%-----------------------------------------------------------------
dec_giop_locate_status_type(0) ->
    'unknown_object';
dec_giop_locate_status_type(1) ->
    'object_here';
dec_giop_locate_status_type(2) ->
    'object_forward';
%% ## IIOP-1.2 ##
dec_giop_locate_status_type(3) ->
    'object_forward_perm';
dec_giop_locate_status_type(4) ->
    'loc_system_exception';
dec_giop_locate_status_type(5) ->
    'loc_needs_addressing_mode'.
 

%%-----------------------------------------------------------------
%% Func: dec_type/5
%%-----------------------------------------------------------------
dec_type(Type, Version, Bytes, Len, ByteOrder) ->
    {Val, Rest, Len2, _} = 
	dec_type(Type, Version, Bytes, Len, ByteOrder, [], 0),
    {Val, Rest, Len2}.

dec_type('tk_null', _Version, Bytes, Len, _, _, C) ->
    {'null', Bytes, Len, C}; 
dec_type('tk_void', _Version, Bytes, Len, _, _, C) ->
    {'ok', Bytes, Len, C}; 
dec_type('tk_short', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 2, C),
    {Short, Rest1} = cdrlib:dec_short(ByteOrder, Rest),
    {Short, Rest1, Len1 + 2, NewC+2};
dec_type('tk_long', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 4, C),
    {Long, Rest1} = cdrlib:dec_long(ByteOrder, Rest),
    {Long, Rest1, Len1 + 4, NewC+4};
dec_type('tk_longlong', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 8, C),
    {Long, Rest1} = cdrlib:dec_longlong(ByteOrder, Rest),
    {Long, Rest1, Len1 + 8, NewC+8};
dec_type('tk_ushort', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 2, C),
    {Short, Rest1} = cdrlib:dec_unsigned_short(ByteOrder, Rest),
    {Short, Rest1, Len1 + 2, NewC+2};
dec_type('tk_ulong', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 4, C),
    {Long, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    {Long, Rest1, Len1 + 4, NewC+4};
dec_type('tk_ulonglong', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 8, C),
    {Long, Rest1} = cdrlib:dec_unsigned_longlong(ByteOrder, Rest),
    {Long, Rest1, Len1 + 8, NewC+8};
dec_type('tk_float', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 4, C),
    {Float, Rest1} = cdrlib:dec_float(ByteOrder, Rest),
    {Float, Rest1, Len1 + 4, NewC+4};
dec_type('tk_double', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 8, C),
    {Double, Rest1} = cdrlib:dec_double(ByteOrder, Rest),
    {Double, Rest1, Len1 + 8, NewC+8};
dec_type('tk_boolean', _Version, Bytes, Len, _, _, C) ->
    {Bool, Rest} = cdrlib:dec_bool(Bytes),
    {Bool, Rest, Len + 1, C+1};
dec_type('tk_char', _Version, Bytes, Len, _, _, C) ->
    {Char, Rest} = cdrlib:dec_char(Bytes),
    {Char, Rest, Len + 1, C+1};
dec_type('tk_wchar', {1,2}, Bytes, Len, _ByteOrder, _, C) ->
    %% For IIOP-1.2 a wchar is almost encoded the same way as an octet-sequence.
    %% The only difference is that the length-value is an octet as well.
    case cdrlib:dec_octet(Bytes) of
	{2, Rest1} ->
	    %% Currently we only allow 2-bytes wchar.
	    {WChar, Rest2} = cdrlib:dec_unsigned_short(big, Rest1),
	    {WChar, Rest2, Len+3, C+3};
	{What, _} ->
	    orber:dbg("[~p] cdr_decode:dec_type(~p); unsupported wchar", 
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'DATA_CONVERSION'{completion_status=?COMPLETED_NO})
    end;
%% For 1.1 the wchar is limited to the use of two-octet fixed-length encoding.
dec_type('tk_wchar', _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 2, C),
    {WChar, Rest2} = cdrlib:dec_unsigned_short(ByteOrder, Rest),
    {WChar, Rest2, Len1 + 2, NewC+2};
dec_type('tk_octet', _Version, Bytes, Len, _, _, C) ->
    {Octet, Rest} = cdrlib:dec_octet(Bytes),
    {Octet, Rest, Len + 1, C+1};
dec_type('tk_any', Version, Bytes, Len, ByteOrder, Buff, C) ->
    {TypeCode, Rest1, Len1, NewC} = dec_type('tk_TypeCode', Version, Bytes, Len, ByteOrder, Buff, C),
    {Value, Rest2, Len2, NewC2} = dec_type(TypeCode, Version, Rest1, Len1, ByteOrder, Buff, NewC),
    {#any{typecode=TypeCode, value=Value}, Rest2, Len2, NewC2};
dec_type('tk_TypeCode', Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_type_code(Version, Bytes, Len, ByteOrder, Buff, C);
dec_type('tk_Principal', Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_sequence(Version, Bytes, 'tk_octet', Len, ByteOrder, Buff, C);
dec_type({'tk_objref', _IFRId, _Name}, Version, Bytes, Len, ByteOrder, Buff, C) -> 
    dec_objref(Version, Bytes, Len, ByteOrder, Buff, C);
dec_type({'tk_struct', IFRId, Name, ElementList}, Version, Bytes, Len, ByteOrder, Buff, C) -> 
    dec_struct(Version, IFRId, Name, ElementList, Bytes, Len, ByteOrder, Buff, C); 
dec_type({'tk_union', IFRId, Name, DiscrTC, Default, ElementList},
	 Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_union(Version, IFRId, Name, DiscrTC, Default, ElementList, Bytes, Len, ByteOrder, Buff, C);
dec_type({'tk_enum', _IFRId, _Name, ElementList}, _Version, Bytes, Len, ByteOrder, _, C) ->
    {Rest, Len1, NewC} = dec_align(Bytes, Len, 4, C),
    {Enum, Rest1} = cdrlib:dec_enum(ByteOrder, ElementList, Rest),
    {Enum, Rest1, Len1 + 4, NewC+4}; 
dec_type({'tk_string', _MaxLength}, Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_string(Version, Bytes, Len, ByteOrder, Buff, C);
dec_type({'tk_wstring', _MaxLength}, Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_wstring(Version, Bytes, Len, ByteOrder, Buff, C);
dec_type({'tk_sequence', ElemTC, _MaxLength}, Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_sequence(Version, Bytes, ElemTC, Len, ByteOrder, Buff, C);
dec_type({'tk_array', ElemTC, Size}, Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_array(Version, Bytes, Size, ElemTC, Len, ByteOrder, Buff, C);
dec_type({'tk_alias', _IFRId, _Name, TC}, Version, Bytes, Len, ByteOrder, Buff, C) ->
    dec_type(TC, Version, Bytes, Len, ByteOrder, Buff, C); 
%dec_type({'tk_except', IFRId, Name, ElementList}, Version, Bytes, Len, ByteOrder) ->
dec_type({'tk_fixed', Digits, Scale}, _Version, Bytes, Len, _ByteOrder, _Buff, C) ->
    dec_fixed(Digits, Scale, Bytes, Len, C);
dec_type(Type, _, _, _, _, _, _) ->
    orber:dbg("[~p] cdr_decode:dec_type(~p)~n"
	      "Incorrect TypeCode or unsupported type.", 
	      [?LINE, Type], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 13), completion_status=?COMPLETED_MAYBE}).

stringify_enum({tk_enum,_,_,_}, Label) ->
    atom_to_list(Label);
stringify_enum(_, Label) ->
    Label.

%%-----------------------------------------------------------------
%% Func: dec_fixed
%%-----------------------------------------------------------------
%% Digits eq. total number of digits.
%% Scale  eq. position of the decimal point.
%% E.g. fixed<5,2> - "123.45"
%% E.g. fixed<4,2> - "12.34"
%% These are encoded as:
%% ## <5,2> ##  ## <4,2> ##
%%     1,2          0,1     eq. 1 octet
%%     3,4          2,3
%%     5,0xC        4,0xC
%%
%% Each number is encoded as a half-octet. Note, for <4,2> a zero is
%% added first to to be able to create "even" octets.
dec_fixed(0, 0, Bytes, Len, C) ->
    {#fixed{digits = 0, scale = 0, value = ""}, Bytes, Len, C};
dec_fixed(Digits, Scale, Bytes, Len, C) ->
    case ?ODD(Digits) of
	true ->
	    {Fixed, Bytes2, Len2, C2, Sign} = dec_fixed_2(Digits, Scale, Bytes, Len, C),
	    case Sign of
		?FIXED_POSITIVE ->
		    {#fixed{digits = Digits, scale = Scale,
			    value = list_to_integer(Fixed)}, Bytes2, Len2, C2};
		?FIXED_NEGATIVE ->
		    {#fixed{digits = Digits, scale = Scale,
			    value = -list_to_integer(Fixed)}, Bytes2, Len2, C2}
	    end;
	false ->
	    %% If the length (of fixed) is even a zero is added first.
	    %% Subtract that we've read 1 digit.
	    <<0:4,D2:4,T/binary>> = Bytes,
	    {Fixed, Bytes2, Len2, C2, Sign} = dec_fixed_2(Digits-1, Scale, T, Len+1, C+1),
	    case Sign of
		?FIXED_POSITIVE ->
		    {#fixed{digits = Digits, scale = Scale, 
			    value = list_to_integer([D2+48|Fixed])}, Bytes2, Len2, C2};
		?FIXED_NEGATIVE ->
		    {#fixed{digits = Digits, scale = Scale, 
			    value = -list_to_integer([D2+48|Fixed])}, Bytes2, Len2, C2}
	    end
    end.

dec_fixed_2(1, _Scale, <<D1:4,?FIXED_POSITIVE:4,T/binary>>, Len, C) ->
    {[D1+48], T, Len+1, C+1, ?FIXED_POSITIVE};
dec_fixed_2(1, _Scale, <<D1:4,?FIXED_NEGATIVE:4,T/binary>>, Len, C) ->
    {[D1+48], T, Len+1, C+1, ?FIXED_NEGATIVE};
dec_fixed_2(Digits, Scale, _Bytes, _Len, _C) when Digits =< 0 ->
    orber:dbg("[~p] cdr_decode:dec_fixed_2(~p, ~p)~n"
	      "Malformed fixed type.", [?LINE, Digits, Scale], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 14), completion_status=?COMPLETED_MAYBE});
dec_fixed_2(Digits, Scale, <<>>, _Len, _C) ->
    orber:dbg("[~p] cdr_decode:dec_fixed_2(~p, ~p)~n"
	      "The fixed type received was to short.", 
	      [?LINE, Digits, Scale], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 14), completion_status=?COMPLETED_MAYBE});
dec_fixed_2(Digits, Scale, <<D1:4,D2:4,T/binary>>, Len, C) ->
    {Seq, Rest2, Len2, NewC2, Sign} = dec_fixed_2(Digits-2, Scale, T, Len+1, C+1),
    {[D1+48, D2+48 | Seq], Rest2, Len2, NewC2, Sign}.

%%-----------------------------------------------------------------
%% Func: dec_sequence/7 and dec_sequence/8
%%-----------------------------------------------------------------
dec_sequence(_Version, Message, 'tk_octet', Len, ByteOrder, _Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    <<OctetSeq:Size/binary, Rest2/binary>> = Rest1,
    {binary_to_list(OctetSeq), Rest2, Len1+4+Size, NewC+4+Size};
dec_sequence(_Version, Message, 'tk_char', Len, ByteOrder, _Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    <<OctetSeq:Size/binary, Rest2/binary>> = Rest1,
    {binary_to_list(OctetSeq), Rest2, Len1+4+Size, NewC+4+Size};
%% We test if it's a sequence of struct's or unions. By doing this we only
%% have to look up the IFR-ID once instead of N times (N eq length of sequence).
dec_sequence(Version, Message, {'tk_struct', IFRId, ShortName, ElementList}, 
	     Len, ByteOrder, Buff, C) when IFRId /= "", ShortName /= "" ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    case IFRId of
	?SYSTEM_TYPE ->
	    dec_sequence_struct(Version, Rest1, Size, ElementList, Len1 + 4, 
				ByteOrder, Buff, NewC+4, ShortName);
	_ ->
	    Name = ifrid_to_name(IFRId, ?IFR_StructDef),
	    dec_sequence_struct(Version, Rest1, Size, ElementList, Len1 + 4, 
				ByteOrder, Buff, NewC+4, Name)
    end;
dec_sequence(Version, Message, 
	     {'tk_union', ?SYSTEM_TYPE, TCName, DiscrTC, Default, ElementList}, 
	     Len, ByteOrder, Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    dec_sequence_union(Version, Rest1, Size, DiscrTC, Default, ElementList, Len1 + 4,
		       ByteOrder, Buff, NewC+4, TCName);
dec_sequence(Version, Message, 
	     {'tk_union', IFRId, _TCName, DiscrTC, Default, ElementList}, 
	     Len, ByteOrder, Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    Name = ifrid_to_name(IFRId, ?IFR_UnionDef),
    dec_sequence_union(Version, Rest1, Size, DiscrTC, Default, ElementList, Len1 + 4,
		       ByteOrder, Buff, NewC+4, Name);
dec_sequence(Version, Message, TypeCode, Len, ByteOrder, Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    dec_sequence(Version, Rest1, Size, TypeCode, Len1 + 4, ByteOrder, Buff, NewC+4).


dec_sequence(_, Message, 0, _Type, Len, _ByteOrder, _Buff, C) ->
    {[], Message, Len, C};
dec_sequence(Version, Message, N, Type, Len, ByteOrder, Buff, C) ->
    {Object, Rest1, Len1, NewC} = dec_type(Type, Version, Message, Len, ByteOrder, Buff, C),
    {Seq, Rest2, Len2, NewC2} = dec_sequence(Version, Rest1, N - 1,  Type, Len1, ByteOrder, Buff, NewC),
    {[Object | Seq], Rest2, Len2, NewC2}.

dec_sequence_struct(_, Message, 0, _Type, Len, _ByteOrder, _Buff, C, _Name) ->
    {[], Message, Len, C};
dec_sequence_struct(Version, Message, N, TypeCodeList, Len, ByteOrder, Buff, C, Name) ->
    {Struct, Rest1, Len1, NewC} = dec_struct1(Version, TypeCodeList, Message, Len, ByteOrder, Buff, C),
    {Seq, Rest2, Len2, NewC2} = dec_sequence_struct(Version, Rest1, N - 1,  TypeCodeList, Len1, ByteOrder, 
						    Buff, NewC, Name),
    {[list_to_tuple([Name |Struct]) | Seq], Rest2, Len2, NewC2}.


dec_sequence_union(_, Message, 0, _DiscrTC, _Default, _ElementList,
		   Len, _ByteOrder, _Buff, C, _Name) ->
    {[], Message, Len, C};
dec_sequence_union(Version, Message, N, DiscrTC, Default, ElementList,
		   Len, ByteOrder, Buff, C, Name) when is_list(ElementList) ->

    {Label, Rest1, Len1, NewC} = dec_type(DiscrTC, Version, Message, Len, ByteOrder, Buff, C),
    Result = dec_union(Version, stringify_enum(DiscrTC, Label), ElementList, Default, 
				     Rest1, Len1, ByteOrder, Buff, NewC),
    {Value, Rest2, Len2, NewC3} = case Result of
		{default, R, L, NewC2} ->
		    dec_union(Version, default, ElementList, Default, 
				     R, L, ByteOrder, Buff, NewC2);
		X ->
		    X
	    end,
    {Seq, Rest3, Len3, NewC4} = dec_sequence_union(Version, Rest2, N - 1, 
						   DiscrTC, Default, ElementList, 
						   Len2, ByteOrder, 
						   Buff, NewC3, Name),
    {[{Name, Label, Value} | Seq], Rest3, Len3, NewC4};
dec_sequence_union(Version, Message, N, _DiscrTC, _Default, Module,
		   Len, ByteOrder, Buff, C, Name) when is_atom(Module) ->
    case catch Module:tc() of
	{tk_union, _, _, DiscrTC, Default, ElementList} ->
	    dec_sequence_union(Version, Message, N, DiscrTC, Default, ElementList,
			       Len, ByteOrder, Buff, C, Name);
	What ->
	    orber:dbg("[~p] ~p:dec_sequence_union(~p). Union module doesn't exist or incorrect.", 
		      [?LINE, ?MODULE, What], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE})
    end.
   
 

%% A special case; when something is encapsulated (i.e. sent as octet-sequence)
%% we sometimes don not want the result to be converted to a list.
dec_octet_sequence_bin(_Version, Message, Len, ByteOrder, _Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    <<OctetSeq:Size/binary, Rest2/binary>> = Rest1,
    {OctetSeq, Rest2, Len1+4+Size, NewC+4+Size}.

%%-----------------------------------------------------------------
%% Func: dec_array/5
%%-----------------------------------------------------------------
dec_array(Version, Message, Size, TypeCode, Len, ByteOrder, Buff, C) ->
    {Seq, Rest1, Len1, NewC} = dec_sequence(Version, Message, Size, TypeCode, Len,
				     ByteOrder, Buff, C),
    {list_to_tuple(Seq), Rest1, Len1, NewC}.


%%-----------------------------------------------------------------
%% Func: dec_string/4
%%-----------------------------------------------------------------
dec_string(_Version, Message, Len, ByteOrder, _Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    if
	Size > 0 ->
	    DataSize = Size-1,
	    <<String:DataSize/binary, _Null:1/binary, Rest2/binary>> = Rest1,
	    {binary_to_list(String), Rest2, Len1+4+Size, NewC+4+Size};
	true ->
	    {"", Rest1, Len1 + 4, NewC+4}
    end.

%%-----------------------------------------------------------------
%% Func: dec_string/4
%%-----------------------------------------------------------------
dec_wstring({1,2}, Message, Len, ByteOrder, Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Octets, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    if
	Octets == 0 ->
	    {"", Rest1, Len1 + 4, NewC+4};
	Octets > 0 ->
	    Size = round(Octets/2),
	    {String, Rest2, Len2, NewC2} = 
		dec_sequence({1,2}, Rest1, Size, 'tk_ushort',
			     Len1 + 4, big, Buff, NewC+4),
	    {String, Rest2, Len2, NewC2};
	true ->
	    orber:dbg("[~p] cdr_decode:dec_wstring(~p);",
		      [?LINE, Rest1], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO})
    end;
dec_wstring(Version, Message, Len, ByteOrder, Buff, C) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, C),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    if
	Size > 0 ->
	    {String, Rest2, Len2, NewC2} = dec_sequence(Version, Rest1, Size - 1, 'tk_wchar',
						 Len1 + 4, ByteOrder, Buff, NewC+4),
	    %% Remove the NULL character.
	    {_, Rest3} = cdrlib:dec_unsigned_short(ByteOrder, Rest2),
	    {String, Rest3, Len2 + 2, NewC2+2};
	Size == 0 ->
	    {"", Rest1, Len1 + 4, NewC+4};
	true ->
	    orber:dbg("[~p] cdr_decode:dec_wstring(~p);",
		      [?LINE, Rest1], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO})
    end.


%%-----------------------------------------------------------------
%% Func: dec_union/9
%%-----------------------------------------------------------------
%% ## NEW IIOP 1.2 ##
dec_union(Version, ?SYSTEM_TYPE, Name, DiscrTC, Default, ElementList, Bytes,
	  Len, ByteOrder, Buff, C) ->
    {Label, Rest1, Len1, NewC} = dec_type(DiscrTC, Version, Bytes, Len, ByteOrder, Buff, C),
    {Value, Rest2, Len2, NewC3} = dec_union(Version, Label, ElementList, Default, 
					    Rest1, Len1, ByteOrder, Buff, NewC),
    {{Name, Label, Value}, Rest2, Len2, NewC3};


dec_union(Version, IFRId, _, DiscrTC, Default, ElementList, Bytes, Len, 
	  ByteOrder, Buff, C) when is_list(ElementList) ->
    {Label, Rest1, Len1, NewC} = dec_type(DiscrTC, Version, Bytes, Len, ByteOrder, Buff, C),
    Result = dec_union(Version, stringify_enum(DiscrTC, Label), ElementList, Default, 
				     Rest1, Len1, ByteOrder, Buff, NewC),
    {Value, Rest2, Len2, NewC3} = case Result of
		{default, R, L, NewC2} ->
		    dec_union(Version, default, ElementList, Default, 
				     R, L, ByteOrder, Buff, NewC2);
		X ->
		    X
	    end,
    Name = ifrid_to_name(IFRId, ?IFR_UnionDef),
    {{Name, Label, Value}, Rest2, Len2, NewC3};
dec_union(Version, IFRId, _, _DiscrTC, _Default, Module, Bytes, Len, 
	  ByteOrder, Buff, C) when is_atom(Module) ->
    case catch Module:tc() of
	{tk_union, _, Name, DiscrTC, Default, ElementList} ->
	    dec_union(Version, IFRId, Name, DiscrTC, Default, ElementList, Bytes, Len, 
		      ByteOrder, Buff, C);
	What ->
	    orber:dbg("[~p] ~p:dec_union(~p). Union module doesn't exist or incorrect.", 
		      [?LINE, ?MODULE, What], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE})
    end.
   
    

dec_union(_, _, [], Default,  Message, Len, _, _Buff, C) when Default < 0 ->
    {undefined, Message, Len, C};
dec_union(_, _, [], _Default, Message, Len, _, _Buff, C) ->
    {default, Message, Len, C};
dec_union(Version, Label, [{Label, _Name, Type}|_List], _Default, Message, Len, ByteOrder, Buff, C) ->
    dec_type(Type, Version, Message, Len, ByteOrder, Buff, C);
dec_union(Version, Label, [_H|List], Default, Message,  Len, ByteOrder, Buff, C) ->
    dec_union(Version, Label, List, Default, Message, Len, ByteOrder, Buff, C).

%%-----------------------------------------------------------------
%% Func: dec_struct/7
%%-----------------------------------------------------------------
dec_struct(Version, "", "", TypeCodeList, Message, Len, ByteOrder, Buff, C) ->
    {Struct, Rest, Len1, NewC} = dec_struct1(Version, TypeCodeList, Message, Len, ByteOrder, Buff, C),
    {list_to_tuple(Struct), Rest, Len1, NewC};
dec_struct(Version, [], Name, TypeCodeList, Message, Len, ByteOrder, Buff, C) -> 
    %% This case is used when communicating with ORB:s which don't supply the IFRId
    %% field in struct type codes (used in any)
    {Struct, Rest, Len1, NewC} = dec_struct1(Version, TypeCodeList, Message, Len, ByteOrder, Buff, C),
    {list_to_tuple([list_to_atom(Name) |Struct]), Rest, Len1, NewC};
dec_struct(Version, ?SYSTEM_TYPE, ShortName, TypeCodeList, Message, Len, ByteOrder, Buff, C) ->
    {Struct, Rest, Len1, NewC} = dec_struct1(Version, TypeCodeList, Message, Len, ByteOrder, Buff, C),
    {list_to_tuple([ShortName |Struct]), Rest, Len1, NewC};
dec_struct(Version, IFRId, _ShortName, TypeCodeList, Message, Len, ByteOrder, Buff, C) ->
    Name = ifrid_to_name(IFRId, ?IFR_StructDef),
    {Struct, Rest, Len1, NewC} = dec_struct1(Version, TypeCodeList, Message, Len, ByteOrder, Buff, C),
    {list_to_tuple([Name |Struct]), Rest, Len1, NewC}.

dec_struct1(_, [], Message, Len, _ByteOrder, _, C) ->
    {[], Message, Len, C};
dec_struct1(Version, [{_ElemName, ElemType} | TypeCodeList], Message, Len, ByteOrder, Buff, C) ->
    {Element, Rest, Len1, NewC} = dec_type(ElemType, Version, Message, Len, ByteOrder, Buff, C),
    {Struct, Rest1, Len2, NewC2} = dec_struct1(Version, TypeCodeList, Rest, Len1, ByteOrder, Buff, NewC),
    {[Element |Struct], Rest1, Len2, NewC2};
dec_struct1(Version, Module, Message, Len, ByteOrder, Buff, C) ->
    case catch Module:tc() of
	{tk_struct, _, _, TypeCodeList} ->
	    dec_struct1(Version, TypeCodeList, Message, Len, ByteOrder, Buff, C);
	What ->
	    orber:dbg("[~p] ~p:dec_struct1(~p). Struct module doesn't exist or incorrect.", 
		      [?LINE, ?MODULE, What], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE})
    end.

ifrid_to_name([], Type) ->
    orber:dbg("[~p] ~p:ifrid_to_name([], ~p). No Id supplied.", 
	      [?LINE, ?MODULE, Type], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?CORBA_OMGVMCID bor 11), 
			   completion_status=?COMPLETED_MAYBE});
ifrid_to_name(Id, Type) -> 
    case orber:light_ifr() of
	true ->
	    orber_ifr:get_module(Id, Type);
	false ->
	    case catch ifrid_to_name_helper(Id, Type) of
		{'EXCEPTION', E} ->
		    corba:raise(E);
		{'EXIT',{aborted,{no_exists,_}}} ->
		    case orber:get_lightweight_nodes() of
			false ->
			    orber:dbg("[~p] cdr_decode:ifrid_to_name(~p, ~p). IFRid not found.", 
				      [?LINE, Id, Type], ?DEBUG_LEVEL),
			    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
			Nodes ->
			    {A,B,C} = now(),
			    random:seed(A,B,C),
			    L = length(Nodes),
			    IFR = get_ifr_node(Nodes, random:uniform(L), L),
			    list_to_atom('OrberApp_IFR':get_absolute_name(IFR, Id))
		    end;
		{'EXIT', Other} ->
		    orber:dbg("[~p] cdr_decode:ifrid_to_name(~p). Unknown: ~p", 
			      [?LINE, Id, Other], ?DEBUG_LEVEL),
		    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
		Name ->
		    list_to_atom(Name)
	    end
    end.
    
ifrid_to_name_helper(Id, ?IFR_UnionDef) ->
    case mnesia:dirty_index_read(ir_UnionDef, Id, #ir_UnionDef.id) of
	[#ir_UnionDef{absolute_name = [$:,$:|N]}] ->
	    change_colons_to_underscore(N, []);
	Other ->
	    orber:dbg("[~p] cdr_decode:ifrid_to_name(~p). IFR Id not found: ~p", 
		      [?LINE, Id, Other], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 9), 
				   completion_status=?COMPLETED_MAYBE})
    end;
ifrid_to_name_helper(Id, ?IFR_StructDef) -> 
    case mnesia:dirty_index_read(ir_StructDef, Id, #ir_StructDef.id) of
	[#ir_StructDef{absolute_name = [$:,$:|N]}] ->
	    change_colons_to_underscore(N, []);
	Other ->
	    orber:dbg("[~p] cdr_decode:ifrid_to_name(~p). IFR Id not found: ~p", 
		      [?LINE, Id, Other], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 10), 
				   completion_status=?COMPLETED_MAYBE})
    end;
ifrid_to_name_helper(Id, ?IFR_ExceptionDef) -> 
    case mnesia:dirty_index_read(ir_ExceptionDef, Id, #ir_ExceptionDef.id) of
	[#ir_ExceptionDef{absolute_name = [$:,$:|N]}] ->
	    change_colons_to_underscore(N, []);
	Other ->
	    orber:dbg("[~p] cdr_decode:ifrid_to_name(~p). IFR Id not found: ~p", 
		      [?LINE, Id, Other], ?DEBUG_LEVEL),
	    corba:raise(#'UNKNOWN'{minor=(?CORBA_OMGVMCID bor 1), 
				   completion_status=?COMPLETED_MAYBE})
    end.

change_colons_to_underscore([$:, $: | T], Acc) ->
    change_colons_to_underscore(T, [$_ |Acc]);
change_colons_to_underscore([H |T], Acc) ->
    change_colons_to_underscore(T, [H |Acc]);
change_colons_to_underscore([], Acc) ->
    lists:reverse(Acc).

get_ifr_node([], _, _) ->
    %% Were not able to contact any of the given nodes.
    orber:dbg("[~p] cdr_decode:get_ifr_node([]). No Node available.", 
			    [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_MAYBE});
get_ifr_node(Nodes, N, L) ->
    Node = lists:nth(N, Nodes),
    case catch corba:resolve_initial_references_remote("OrberIFR", [Node]) of
	IFR when is_record(IFR, 'IOP_IOR') ->
	    IFR;
	_ ->
	    %% Not able to commincate with the node. Try next one.
	    NewL = L-1,
	    get_ifr_node(lists:delete(Node, Nodes), random:uniform(NewL), NewL)
    end.

    
%%-----------------------------------------------------------------
%% Func: dec_objref/4
%%-----------------------------------------------------------------
dec_objref(Version, Message, Len, ByteOrder) ->
    dec_objref(Version, Message, Len, ByteOrder, [], 0).
dec_objref(Version, Message, Len, ByteOrder, _Buff, C) ->
    {IOR, Rest, Length} = iop_ior:decode(Version, Message, Len, ByteOrder),
    {IOR, Rest, Length, C+Length-Len}.

%%-----------------------------------------------------------------
%% Func: dec_system_exception/4 and dec_user_exception/4
%%-----------------------------------------------------------------
dec_system_exception(Version, Message, Len, ByteOrder) when Version == {1,2} ->
    {Rest0, Len0, _Counter} = dec_align(Message, Len, 8, Len),
    {TypeId, Rest1, Len1} = dec_type({'tk_string', 0}, Version, Rest0, Len0, ByteOrder),
    Name = orber_exceptions:get_name(TypeId, ?SYSTEM_EXCEPTION),
    {Struct, _Rest2, Len2} = 
	dec_exception_1(Version, [{"minor",'tk_ulong'},
				  {"completed",
				   {'tk_enum', "", "completion_status",
				    ["COMPLETED_YES", "COMPLETED_NO",
				     "COMPLETED_MAYBE"]}}],
			Rest1, Len1, ByteOrder),
    {list_to_tuple([Name, "" |Struct]), Len2};
dec_system_exception(Version, Message, Len, ByteOrder) ->
    {TypeId, Rest1, Len1} = dec_type({'tk_string', 0}, Version, Message, Len, ByteOrder),
    Name = orber_exceptions:get_name(TypeId, ?SYSTEM_EXCEPTION),
    {Struct, _Rest2, Len2} = 
	dec_exception_1(Version, [{"minor",'tk_ulong'},
				  {"completed",
				   {'tk_enum', "", "completion_status",
				    ["COMPLETED_YES", "COMPLETED_NO",
				     "COMPLETED_MAYBE"]}}],
			Rest1, Len1, ByteOrder),
    {list_to_tuple([Name, "" |Struct]), Len2}.

dec_user_exception(Version, Message, Len, ByteOrder) when Version == {1,2} ->
    {Rest0, Len0, _Counter} = dec_align(Message, Len, 8, Len),
    {TypeId, Rest1, Len1} = dec_type({'tk_string', 0}, Version, Rest0, Len0, ByteOrder),
    Name = ifrid_to_name(TypeId, ?IFR_ExceptionDef),
    {'tk_except', _, _, ElementList} = get_user_exception_type(TypeId),
    {Struct, _Rest2, Len2} = dec_exception_1(Version, ElementList, Rest1, Len1,
					     ByteOrder),
    {list_to_tuple([Name, TypeId |Struct]), Len2};
dec_user_exception(Version, Message, Len, ByteOrder) ->
    {TypeId, Rest1, Len1} = dec_type({'tk_string', 0}, Version, Message, Len, ByteOrder),
    Name = ifrid_to_name(TypeId, ?IFR_ExceptionDef),
    {'tk_except', _, _, ElementList} = get_user_exception_type(TypeId),
    {Struct, _Rest2, Len2} = dec_exception_1(Version, ElementList, Rest1, Len1,
					     ByteOrder),
    {list_to_tuple([Name, TypeId |Struct]), Len2}.

dec_exception_1(_, [], Message, Len, _ByteOrder) ->
    {[], Message, Len};
dec_exception_1(Version, [{_ElemName, ElemType} | ElementList], Message,
		Len, ByteOrder) ->
    {Element, Rest, Len1} = dec_type(ElemType, Version, Message, Len, ByteOrder),
    {Struct, Rest1, Len2} = dec_exception_1(Version, ElementList, Rest, Len1,
						   ByteOrder),
    {[Element |Struct], Rest1, Len2}.


get_user_exception_type(TypeId) ->
    case orber:light_ifr() of
	true ->
	    orber_ifr:get_tc(TypeId, ?IFR_ExceptionDef);
	false ->
	    case orber:get_lightweight_nodes() of
		false ->
		    case mnesia:dirty_index_read(ir_ExceptionDef, TypeId,
						 #ir_ExceptionDef.id) of
			[ExcDef] when is_record(ExcDef, ir_ExceptionDef) ->  
			    ExcDef#ir_ExceptionDef.type;
			Other ->
			    orber:dbg("[~p] cdr_decode:get_user_exception_type(~p). IFR Id not found: ~p", 
				      [?LINE, TypeId, Other], ?DEBUG_LEVEL),
			    corba:raise(#'UNKNOWN'{minor=(?CORBA_OMGVMCID bor 1), 
						   completion_status=?COMPLETED_MAYBE})
		    end;
		Nodes ->
		    {A,B,C} = now(),
		    random:seed(A,B,C),
		    L = length(Nodes),
		    IFR = get_ifr_node(Nodes, random:uniform(L), L),
		    'OrberApp_IFR':get_user_exception_type(IFR, TypeId)
	    end
    end.

%%-----------------------------------------------------------------
%% Func: dec_type_code/4
%%-----------------------------------------------------------------
dec_type_code(Version, Message, Len, ByteOrder, Buff, C) ->
    {TypeNo, Message1, Len1, NewC} = dec_type('tk_ulong', Version, Message, Len, ByteOrder, Buff, C),
    TC = dec_type_code(TypeNo, Version, Message1, Len1, ByteOrder, Buff, NewC),
    erase(orber_indirection),
    TC.

%%-----------------------------------------------------------------
%% Func: dec_type_code/5
%%-----------------------------------------------------------------
dec_type_code(0, _, Message, Len, _, _, C) ->
    {'tk_null', Message, Len, C};
dec_type_code(1, _, Message, Len, _, _, C) ->
    {'tk_void', Message, Len, C};
dec_type_code(2, _, Message, Len, _, _, C) ->
    {'tk_short', Message, Len, C};
dec_type_code(3, _, Message, Len, _, _, C) ->
    {'tk_long', Message, Len, C};
dec_type_code(23, _, Message, Len, _, _, C) ->
    {'tk_longlong', Message, Len, C};
dec_type_code(25, _, Message, Len, _, _, C) ->
    {'tk_longdouble', Message, Len, C};
dec_type_code(4, _, Message, Len, _, _, C) ->
    {'tk_ushort', Message, Len, C};
dec_type_code(5, _, Message, Len, _, _, C) ->
    {'tk_ulong', Message, Len, C};
dec_type_code(24, _, Message, Len, _, _, C) ->
    {'tk_ulonglong', Message, Len, C};
dec_type_code(6, _, Message, Len, _, _, C) ->
    {'tk_float', Message, Len, C};
dec_type_code(7, _, Message, Len, _, _, C) ->
    {'tk_double', Message, Len, C};
dec_type_code(8, _, Message, Len, _, _, C) ->
    {'tk_boolean', Message, Len, C};
dec_type_code(9, _, Message, Len, _, _, C) ->
    {'tk_char', Message, Len, C};
dec_type_code(26, _, Message, Len, _, _, C) ->
    {'tk_wchar', Message, Len, C};
dec_type_code(10, _, Message, Len, _, _, C) ->
    {'tk_octet', Message, Len, C};
dec_type_code(11, _, Message, Len, _, _, C) ->
    {'tk_any', Message, Len, C};
dec_type_code(12, _, Message, Len, _, _, C) ->
    {'tk_TypeCode', Message, Len, C};
dec_type_code(13, _, Message, Len, _, _, C) ->
    {'tk_Principal', Message, Len, C};
dec_type_code(14, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg get the byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name}, <<>>, _Len2, NewC} = 
	dec_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
					{"name", {'tk_string', 0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_objref', RepId, Name}, Message1, Len1, NewC};
dec_type_code(15, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg get the byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name, ElementList}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "",
		  [{"repository ID", {'tk_string', 0}},
		   {"name", {'tk_string', 0}},
		   {"element list",
		    {'tk_sequence', {'tk_struct', "","",
				     [{"member name", {'tk_string', 0}},
				      {"member type", 'tk_TypeCode'}]},
		     0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_struct', RepId, Name, ElementList}, Message1, Len1, NewC};
dec_type_code(16, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg get the byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name, DiscrTC, Default}, Rest2, RestLen2, NewC} =
	dec_type({'tk_struct', "", "",
		  [{"repository ID", {'tk_string', 0}},
		   {"name", {'tk_string', 0}},
		   {"discriminant type", 'tk_TypeCode'},
		   {"default used", 'tk_long'}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {ElementList, <<>>, _RestLen3, NewC2} =
	dec_type({'tk_sequence', {'tk_struct', "","",
				  [{"label value", DiscrTC},
				   {"member name", {'tk_string', 0}},
				   {"member type", 'tk_TypeCode'}]}, 0},
		 Version, Rest2, RestLen2, ByteOrder1, Buff, NewC),
    NewElementList =
	case check_enum(DiscrTC) of
	    true ->
		lists:map(fun({L,N,T}) -> {atom_to_list(L),N,T} end, ElementList);
	    false ->
		ElementList
	end,
    {{'tk_union', RepId, Name, DiscrTC, Default, NewElementList}, Message1, Len1, NewC2};
dec_type_code(17, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg get the byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name, ElementList}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "",
		  [{"repository ID", {'tk_string', 0}},
		   {"name", {'tk_string', 0}},
		   {"element list",
		    {'tk_sequence', {'tk_string', 0}, 0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_enum', RepId, Name, ElementList}, Message1, Len1, NewC};
dec_type_code(18, Version, Message, Len, ByteOrder, Buff, C) ->
    {MaxLength, Message1, Len1, NewC} =
	dec_type('tk_ulong', Version, Message, Len, ByteOrder, Buff, C),
    {{'tk_string', MaxLength}, Message1, Len1, NewC};
dec_type_code(19, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg get the byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{ElemTC, MaxLength}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"element type", 'tk_TypeCode'},
					{"max length", 'tk_ulong'}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_sequence', ElemTC, MaxLength}, Message1, Len1, NewC};
dec_type_code(20, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg get the byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{ElemTC, Length}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"element type", 'tk_TypeCode'},
					{"length", 'tk_ulong'}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_array', ElemTC, Length}, Message1, Len1, NewC};
dec_type_code(21, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg ge a byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name, TC}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
					{"name", {'tk_string', 0}},
					{"TypeCode", 'tk_TypeCode'}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_alias', RepId, Name, TC}, Message1, Len1, NewC};
dec_type_code(22, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    %% Decode marshalled parameters, eg get the byteorder first
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name, ElementList}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "",
		  [{"repository ID", {'tk_string', 0}},
		   {"name", {'tk_string', 0}},
		   {"element list",
		    {'tk_sequence', {'tk_struct', "","",
				     [{"member name", {'tk_string', 0}},
				      {"member type", 'tk_TypeCode'}]},
		     0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_except', RepId, Name, ElementList}, Message1, Len1, NewC};
dec_type_code(27, Version, Message, Len, ByteOrder, Buff, C) ->
    {MaxLength, Message1, Len1, NewC} =
	dec_type('tk_ulong', Version, Message, Len, ByteOrder, Buff, C),
    {{'tk_wstring', MaxLength}, Message1, Len1, NewC};
dec_type_code(28, Version, Message, Len, ByteOrder, Buff, C) ->
    {Digits, Message1, Len1, C1} =
	dec_type('tk_ushort', Version, Message, Len, ByteOrder, Buff, C),
    {Scale, Message2, Len2, C2} =
	dec_type('tk_short', Version, Message1, Len1, ByteOrder, Buff, C1),
    {{'tk_fixed', Digits, Scale}, Message2, Len2, C2};
dec_type_code(29, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name, ValueModifier, TC, ElementList}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
					{"name", {'tk_string', 0}},
					{"ValueModifier", 'tk_short'},
					{"TypeCode", 'tk_TypeCode'},
					{"element list",
					 {'tk_sequence', 
					  {'tk_struct', "","",
					   [{"member name", {'tk_string', 0}},
					    {"member type", 'tk_TypeCode'},
					    {"Visibility", 'tk_short'}]},
					  0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_value', RepId, Name, ValueModifier, TC, ElementList}, Message1, Len1, NewC};
dec_type_code(30, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name, TC}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
					{"name", {'tk_string', 0}},
					{"TypeCode", 'tk_TypeCode'}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_value_box', RepId, Name, TC}, Message1, Len1, NewC};
dec_type_code(31, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
					{"name", {'tk_string', 0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_native', RepId, Name}, Message1, Len1, NewC};
dec_type_code(32, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"RepositoryId", {'tk_string', 0}},
					{"name", {'tk_string', 0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_abstract_interface', RepId, Name}, Message1, Len1, NewC};
dec_type_code(33, Version, Message, Len, ByteOrder, Buff, C) ->
    {ComplexParams, Message1, Len1, Ex} = decode_complex_tc_parameters(Version, Message, Len, ByteOrder),
    {ByteOrder1, Rest1} = dec_byte_order(ComplexParams),
    {{RepId, Name}, <<>>, _Len2, NewC} =
	dec_type({'tk_struct', "", "", [{"RepositoryId", {'tk_string', 0}},
					{"name", {'tk_string', 0}}]},
		 Version, Rest1, 1, ByteOrder1, Buff, C+1+Ex),
    {{'tk_local_interface', RepId, Name}, Message1, Len1, NewC};
dec_type_code(16#ffffffff, Version, Message, Len, ByteOrder, Buff, C) ->
    {Indirection, Message1, Len1, NewC} =
	dec_type('tk_long', Version, Message, Len, ByteOrder, Buff, C),
    Position = C+Indirection,
    case put(orber_indirection, Position) of
	Position ->
%%	    {{'none', Indirection}, Message1, Len1, NewC};
            %% Recursive TypeCode. Break the loop.
 	    orber:dbg("[~p] cdr_decode:dec_type_code(~p); Recursive TC not supported.", 
 		      [?LINE,Position], ?DEBUG_LEVEL),
 	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	_ ->
	    <<_:Position/binary, SubBuff/binary>> = Buff,
	    {TC, _, _, _} = dec_type_code(Version, SubBuff, Position, ByteOrder, Buff, Position),
	    {TC, Message1, Len1, NewC}
    end;
dec_type_code(Type, _, _, _, _, _, _) -> 
    orber:dbg("[~p] cdr_decode:dec_type_code(~p); No match.", 
			    [?LINE, Type], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 8), completion_status=?COMPLETED_MAYBE}).

check_enum({'tk_enum', _, _, _}) ->
    true;
check_enum(_) ->
    false.


decode_complex_tc_parameters(_Version, Message, Len, ByteOrder) ->
    {Rest, Len1, NewC} = dec_align(Message, Len, 4, 0),
    {Size, Rest1} = cdrlib:dec_unsigned_long(ByteOrder, Rest),
    <<OctetSeq:Size/binary, Rest2/binary>> = Rest1,
    {OctetSeq, Rest2, Len1+4+Size, NewC+4}.

%%-----------------------------------------------------------------
%% Func: dec_align/3
%% Args: 
%%       R - The byte sequence that shall be aligned.
%%       Len - The number of bytes read so far.
%%       Alignment - The alignment as an integer (for example: 2,4,8).
%% Returns: 
%%       An aligned byte sequence.
%%-----------------------------------------------------------------
dec_align(R, Len, Alignment, C) ->
    Rem = Len rem Alignment,
    if Rem == 0 ->
	    {R, Len, C};
       true ->
	    Diff = Alignment - Rem,
	    <<_:Diff/binary,Rest/binary>> = R,
	    {Rest, Len + Diff, C + Diff}
    end.

%%---------------- EOF MODULE ----------------------------------------
