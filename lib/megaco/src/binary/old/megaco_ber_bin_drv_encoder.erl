%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(megaco_ber_bin_drv_encoder).

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

-define(V1_ASN1_MOD,     megaco_ber_bin_drv_media_gateway_control_v1).
-define(V2_ASN1_MOD,     megaco_ber_bin_drv_media_gateway_control_v2).
-define(V3_ASN1_MOD,     megaco_ber_bin_drv_media_gateway_control_v3).
-define(PREV3A_ASN1_MOD, megaco_ber_bin_drv_media_gateway_control_prev3a).
-define(PREV3B_ASN1_MOD, megaco_ber_bin_drv_media_gateway_control_prev3b).
-define(PREV3C_ASN1_MOD, megaco_ber_bin_drv_media_gateway_control_prev3c).

-define(V1_TRANS_MOD,     megaco_binary_transformer_v1).
-define(V2_TRANS_MOD,     megaco_binary_transformer_v2).
-define(V3_TRANS_MOD,     megaco_binary_transformer_v3).
-define(PREV3A_TRANS_MOD, megaco_binary_transformer_prev3a).
-define(PREV3B_TRANS_MOD, megaco_binary_transformer_prev3b).
-define(PREV3C_TRANS_MOD, megaco_binary_transformer_prev3c).

-define(BIN_LIB, megaco_binary_encoder_lib).


%%----------------------------------------------------------------------
%% Detect (check) which version a message is
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------
 
version_of([{version3,v3}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?V3_ASN1_MOD], 
    ?BIN_LIB:version_of(EC, Binary, dynamic, Decoders);
version_of([{version3,prev3c}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3C_ASN1_MOD], 
    ?BIN_LIB:version_of(EC, Binary, dynamic, Decoders);
version_of([{version3,prev3b}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3B_ASN1_MOD], 
    ?BIN_LIB:version_of(EC, Binary, dynamic, Decoders);
version_of([{version3,prev3a}|EC], Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3A_ASN1_MOD], 
    ?BIN_LIB:version_of(EC, Binary, dynamic, Decoders);
version_of(EC, Binary) ->
    Decoders = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?V3_ASN1_MOD], 
    ?BIN_LIB:version_of(EC, Binary, dynamic, Decoders).
    

%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------


encode_message(EC,  
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EC, V, MegaMsg).

encode_message([{version3,_}|EC], 1, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, ?V1_ASN1_MOD, ?V1_TRANS_MOD, io_list);
encode_message(EC, 1, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, ?V1_ASN1_MOD, ?V1_TRANS_MOD, io_list);
encode_message([{version3,_}|EC], 2, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, ?V2_ASN1_MOD, ?V2_TRANS_MOD, io_list);
encode_message(EC, 2, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, ?V2_ASN1_MOD, ?V2_TRANS_MOD, io_list);
encode_message([{version3,v3}|EC], 3, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, ?V3_ASN1_MOD, ?V3_TRANS_MOD, io_list);
encode_message([{version3,prev3c}|EC], 3, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, 
			    ?PREV3C_ASN1_MOD, ?PREV3C_TRANS_MOD, io_list);
encode_message([{version3,prev3b}|EC], 3, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, 
			    ?PREV3B_ASN1_MOD, ?PREV3B_TRANS_MOD, io_list);
encode_message([{version3,prev3a}|EC], 3, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, 
			    ?PREV3A_ASN1_MOD, ?PREV3A_TRANS_MOD, io_list);
encode_message(EC, 3, MegaMsg) ->
    ?BIN_LIB:encode_message(EC, MegaMsg, ?V3_ASN1_MOD, ?V3_TRANS_MOD, io_list).


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_transaction(EC, 1, Trans) ->
    %%     ?BIN_LIB:encode_transaction(EC, Trans,
    %% 					     ?V1_ASN1_MOD, 
    %% 					     ?V1_TRANS_MOD,
    %% 					     io_list);
    {error, not_implemented};
encode_transaction(EC, 2, Trans) ->
    %%     ?BIN_LIB:encode_transaction(EC, Trans, 
    %% 					     ?V2_ASN1_MOD, 
    %% 					     ?V2_TRANS_MOD,
    %% 					     io_list).
    {error, not_implemented};
encode_transaction(EC, 3, Trans) ->
    %%     ?BIN_LIB:encode_transaction(EC, Trans, 
    %% 					     ?V3_ASN1_MOD, 
    %% 					     ?V3_TRANS_MOD,
    %% 					     io_list).
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests(EC, 1, ActReqs) when is_list(ActReqs) ->
    %%     ?BIN_LIB:encode_action_requests(EC, ActReqs,
    %% 						 ?V1_ASN1_MOD, 
    %% 						 ?V1_TRANS_MOD,
    %% 						 io_list);
    {error, not_implemented};
encode_action_requests(EC, 2, ActReqs) when is_list(ActReqs) ->
    %%     ?BIN_LIB:encode_action_requests(EC, ActReqs,
    %% 						 ?V2_ASN1_MOD, 
    %% 						 ?V2_TRANS_MOD,
    %% 						 io_list).
    {error, not_implemented};
encode_action_requests(EC, 3, ActReqs) when is_list(ActReqs) ->
    %%     ?BIN_LIB:encode_action_requests(EC, ActReqs,
    %% 						 ?V3_ASN1_MOD, 
    %% 						 ?V3_TRANS_MOD,
    %% 						 io_list).
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request(EC, 1, ActReq) ->
    %%     ?BIN_LIB:encode_action_request(EC, ActReq,
    %% 						?V1_ASN1_MOD, 
    %% 						?V1_TRANS_MOD,
    %% 						io_list);
    {error, not_implemented};
encode_action_request(EC, 2, ActReq) ->
    %%     ?BIN_LIB:encode_action_request(EC, ActReq,
    %% 						?V2_ASN1_MOD, 
    %% 						?V2_TRANS_MOD,
    %% 						io_list).
    {error, not_implemented};
encode_action_request(EC, 3, ActReq) ->
    %%     ?BIN_LIB:encode_action_request(EC, ActReq,
    %% 						?V3_ASN1_MOD, 
    %% 						?V3_TRANS_MOD,
    %% 						io_list).
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

%% Old decode function
decode_message(EC, Binary) ->
    decode_message(EC, 1, Binary).

%% Select from message
%% This does not work at the moment so, we use version 1 for this
decode_message([{version3,v3}|EC], dynamic, Binary) ->
    Mods = [{?V1_ASN1_MOD, ?V1_TRANS_MOD},
	    {?V2_ASN1_MOD, ?V2_TRANS_MOD}, 
	    {?V3_ASN1_MOD, ?V3_TRANS_MOD}], 
    ?BIN_LIB:decode_message_dynamic(EC, Binary, Mods, binary);
decode_message([{version3,prev3c}|EC], dynamic, Binary) ->
    Mods = [{?V1_ASN1_MOD,     ?V1_TRANS_MOD},
	    {?V2_ASN1_MOD,     ?V2_TRANS_MOD}, 
	    {?PREV3C_ASN1_MOD, ?PREV3C_TRANS_MOD}], 
    ?BIN_LIB:decode_message_dynamic(EC, Binary, Mods, binary);
decode_message([{version3,prev3b}|EC], dynamic, Binary) ->
    Mods = [{?V1_ASN1_MOD,     ?V1_TRANS_MOD},
	    {?V2_ASN1_MOD,     ?V2_TRANS_MOD}, 
	    {?PREV3B_ASN1_MOD, ?PREV3B_TRANS_MOD}], 
    ?BIN_LIB:decode_message_dynamic(EC, Binary, Mods, binary);
decode_message([{version3,prev3a}|EC], dynamic, Binary) ->
    Mods = [{?V1_ASN1_MOD,     ?V1_TRANS_MOD},
	    {?V2_ASN1_MOD,     ?V2_TRANS_MOD}, 
	    {?PREV3A_ASN1_MOD, ?PREV3A_TRANS_MOD}], 
    ?BIN_LIB:decode_message_dynamic(EC, Binary, Mods, binary);
decode_message(EC, dynamic, Binary) ->
    Mods = [{?V1_ASN1_MOD, ?V1_TRANS_MOD},
	    {?V2_ASN1_MOD, ?V2_TRANS_MOD}, 
	    {?V3_ASN1_MOD, ?V3_TRANS_MOD}], 
    ?BIN_LIB:decode_message_dynamic(EC, Binary, Mods, binary);

decode_message([{version3,_}|EC], 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message(EC, 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);

decode_message([{version3,_}|EC], 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
decode_message(EC, 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD, 
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);

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
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);


decode_mini_message([{version3,v3}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?V3_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message([{version3,prev3c}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3C_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message([{version3,prev3b}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3B_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message([{version3,prev3a}|EC], dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?PREV3A_ASN1_MOD], 
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
decode_mini_message(EC, dynamic, Bin) ->
    Mods = [?V1_ASN1_MOD, ?V2_ASN1_MOD, ?V3_ASN1_MOD], 
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
