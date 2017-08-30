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
%%----------------------------------------------------------------------
%% Purpose : Handle ASN.1 BER encoding of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_ber_encoder).

-behaviour(megaco_encoder).

-export([encode_message/3, decode_message/3,
	 decode_mini_message/3, 

	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3,
	 encode_action_reply/3,

	 version_of/2]).

%% Backward compatible functions:
-export([encode_message/2, decode_message/2]).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").

-define(V1_ASN1_MOD,     megaco_ber_media_gateway_control_v1).
-define(V2_ASN1_MOD,     megaco_ber_media_gateway_control_v2).
-define(V3_ASN1_MOD,     megaco_ber_media_gateway_control_v3).
-define(PREV3A_ASN1_MOD, megaco_ber_media_gateway_control_prev3a).
-define(PREV3B_ASN1_MOD, megaco_ber_media_gateway_control_prev3b).
-define(PREV3C_ASN1_MOD, megaco_ber_media_gateway_control_prev3c).

-define(V1_TRANS_MOD,     megaco_binary_transformer_v1).
-define(V2_TRANS_MOD,     megaco_binary_transformer_v2).
-define(V3_TRANS_MOD,     megaco_binary_transformer_v3).
-define(PREV3A_TRANS_MOD, megaco_binary_transformer_prev3a).
-define(PREV3B_TRANS_MOD, megaco_binary_transformer_prev3b).
-define(PREV3C_TRANS_MOD, megaco_binary_transformer_prev3c).

-define(BIN_LIB, megaco_binary_encoder_lib).


%%----------------------------------------------------------------------
%% Detect (check/get) message version
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------

version_of([{version3,v3}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?V3_ASN1_MOD],
    ?BIN_LIB:version_of(EC, Binary, 1, Decoders);
version_of([{version3,prev3c}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3C_ASN1_MOD],
    ?BIN_LIB:version_of(EC, Binary, 1, Decoders);
version_of([{version3,prev3b}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3B_ASN1_MOD],
    ?BIN_LIB:version_of(EC, Binary, 1, Decoders);
version_of([{version3,prev3a}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3A_ASN1_MOD],
    ?BIN_LIB:version_of(EC, Binary, 1, Decoders);
version_of(EC, Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?V3_ASN1_MOD],
    ?BIN_LIB:version_of(EC, Binary, 1, Decoders).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EC, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EC, V, MegaMsg).


%% -- Version 1 --

encode_message([{version3,_}|EC], 1, MegaMsg) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
encode_message(EC, 1, MegaMsg) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);


%% -- Version 2 --

encode_message([{version3,_}|EC], 2, MegaMsg) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
encode_message(EC, 2, MegaMsg) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);


%% -- Version 3 --

encode_message([{version3,v3}|EC], 3, MegaMsg) ->
    AsnMod   = ?V3_ASN1_MOD, 
    TransMod = ?V3_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
encode_message([{version3,prev3c}|EC], 3, MegaMsg) ->
    AsnMod   = ?PREV3C_ASN1_MOD, 
    TransMod = ?PREV3C_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
encode_message([{version3,prev3b}|EC], 3, MegaMsg) ->
    AsnMod   = ?PREV3B_ASN1_MOD, 
    TransMod = ?PREV3B_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
encode_message([{version3,prev3a}|EC], 3, MegaMsg) ->
    AsnMod   = ?PREV3A_ASN1_MOD, 
    TransMod = ?PREV3A_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
encode_message(EC, 3, MegaMsg) ->
    AsnMod   = ?V3_ASN1_MOD, 
    TransMod = ?V3_TRANS_MOD,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_transaction(_EC, 1, _Trans) ->
%%     AsnMod   = ?V1_ASN1_MOD, 
%%     TransMod = ?V1_TRANS_MOD,
%%     ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, 
%% 					     io_list);
    {error, not_implemented};
encode_transaction(_EC, 2, _Trans) ->
%%     AsnMod   = ?V2_ASN1_MOD, 
%%     TransMod = ?V2_TRANS_MOD,
%%     ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, 
%% 					     io_list);
    {error, not_implemented};
encode_transaction(_EC, 3, _Trans) ->
%%     AsnMod   = ?V3_ASN1_MOD, 
%%     TransMod = ?V3_TRANS_MOD,
%%     ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, 
%% 					     io_list);
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests(_EC, 1, ActReqs) when is_list(ActReqs) ->
%%     AsnMod   = ?V1_ASN1_MOD, 
%%     TransMod = ?V1_TRANS_MOD,
%%     ?BIN_LIB:encode_action_requests(EC, ActReqs,
%% 						    AsnMod, TransMod, 
%% 						    io_list);
    {error, not_implemented};
encode_action_requests(_EC, 2, ActReqs) when is_list(ActReqs) ->
%%     AsnMod   = ?V2_ASN1_MOD, 
%%     TransMod = ?V2_TRANS_MOD,
%%     ?BIN_LIB:encode_action_requests(EC, ActReqs,
%% 						    AsnMod, TransMod, 
%% 						    io_list);
    {error, not_implemented};
encode_action_requests(_EC, 3, ActReqs) when is_list(ActReqs) ->
%%     AsnMod   = ?V3_ASN1_MOD, 
%%     TransMod = ?V3_TRANS_MOD,
%%     ?BIN_LIB:encode_action_requests(EC, ActReqs,
%% 						    AsnMod, TransMod, 
%% 						    io_list);
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request(_EC, 1, _ActReq) ->
%%     AsnMod   = ?V1_ASN1_MOD, 
%%     TransMod = ?V1_TRANS_MOD,
%%     ?BIN_LIB:encode_action_request(EC, ActReq,
%% 						   AsnMod, TransMod, 
%% 						   io_list);
    {error, not_implemented};
encode_action_request(_EC, 2, _ActReq) ->
%%     AsnMod   = ?V2_ASN1_MOD, 
%%     TransMod = ?V2_TRANS_MOD,
%%     ?BIN_LIB:encode_action_request(EC, ActReq,
%% 						   AsnMod, TransMod, 
%% 						   io_list);
    {error, not_implemented};
encode_action_request(_EC, 3, _ActReq) ->
%%     AsnMod   = ?V3_ASN1_MOD, 
%%     TransMod = ?V3_TRANS_MOD,
%%     ?BIN_LIB:encode_action_request(EC, ActReq,
%% 						   AsnMod, TransMod, 
%% 						   io_list);
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a action reply into a deep io list
%% Not yest supported by this binary codec!
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_action_reply(_EC, _V, _AcionReply) ->
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(EC, Binary) ->
    decode_message(EC, 1, Binary).

%% Not supported for this codec, revert to version 1
decode_message(EC, dynamic, Binary) ->
    decode_message(EC, 1, Binary);


%% -- Version 1 --
 
decode_message([{version3,_}|EC], 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message(EC, 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);


%% -- Version 2 --

decode_message([{version3,_}|EC], 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message(EC, 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);


%% -- Version 3 --

decode_message([{version3,v3}|EC], 3, Binary) ->
    AsnMod   = ?V3_ASN1_MOD, 
    TransMod = ?V3_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message([{version3,prev3c}|EC], 3, Binary) ->
    AsnMod   = ?PREV3C_ASN1_MOD, 
    TransMod = ?PREV3C_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message([{version3,prev3b}|EC], 3, Binary) ->
    AsnMod   = ?PREV3B_ASN1_MOD, 
    TransMod = ?PREV3B_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message([{version3,prev3a}|EC], 3, Binary) ->
    AsnMod   = ?PREV3A_ASN1_MOD, 
    TransMod = ?PREV3A_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message(EC, 3, Binary) ->
    AsnMod   = ?V3_ASN1_MOD, 
    TransMod = ?V3_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary).


decode_mini_message([{version3,v3}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, 
	    ?V2_ASN1_MOD, 
	    ?V3_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message([{version3,prev3c}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, 
	    ?V2_ASN1_MOD, 
	    ?PREV3C_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message([{version3,prev3b}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, 
	    ?V2_ASN1_MOD, 
	    ?PREV3B_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message([{version3,prev3a}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, 
	    ?V2_ASN1_MOD, 
	    ?PREV3A_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message(EC, dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, 
	    ?V2_ASN1_MOD, 
	    ?V3_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);

decode_mini_message([{version3,_}|EC], 1, Bin) ->
    AsnMod = ?V1_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message(EC, 1, Bin) ->
    AsnMod = ?V1_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message([{version3,_}|EC], 2, Bin) ->
    AsnMod = ?V2_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message(EC, 2, Bin) ->
    AsnMod = ?V2_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message([{version3,v3}|EC], 3, Bin) ->
    AsnMod = ?V3_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message([{version3,prev3c}|EC], 3, Bin) ->
    AsnMod = ?PREV3C_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message([{version3,prev3b}|EC], 3, Bin) ->
    AsnMod = ?PREV3B_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message([{version3,prev3a}|EC], 3, Bin) ->
    AsnMod = ?PREV3A_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
decode_mini_message(EC, 3, Bin) ->
    AsnMod = ?V3_ASN1_MOD, 
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary).


