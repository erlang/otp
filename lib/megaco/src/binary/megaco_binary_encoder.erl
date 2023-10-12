%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2020. All Rights Reserved.
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

-module(megaco_binary_encoder).

-behaviour(megaco_encoder).

%% API
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

-define(BIN_LIB, megaco_binary_encoder_lib).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EC, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EC, V, MegaMsg).

%% <BACKWARD-COMPAT-CLAUSE>
encode_message([{version3,_}|EC], 1, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_message(EC, 1, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_message([{version3,_}|EC], 2, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_message(EC, 2, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_message([{version3,v3}|EC], 3, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_message(EC, 3, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_message(EC, MegaMsg, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

%% <BACKWARD-COMPAT-CLAUSE>
encode_transaction([{version3,_}|EC], 1, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_transaction(EC, 1, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_transaction([{version3,_}|EC], 2, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_transaction(EC, 2, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_transaction([{version3,v3}|EC], 3, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_transaction(EC, 3, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_transaction(EC, Trans, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_requests([{version3,_}|EC], 1, ActReqs) 
  when is_list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_requests(EC, 1, ActReqs) when is_list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_requests([{version3,_}|EC], 2, ActReqs) 
  when is_list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_requests(EC, 2, ActReqs) when is_list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_requests([{version3,v3}|EC], 3, ActReqs) 
  when is_list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_requests(EC, 3, ActReqs) when is_list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_request([{version3,_}|EC], 1, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_action_request(EC, ActReq, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_request(EC, 1, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:encode_action_request(EC, ActReq, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_request([{version3,_}|EC], 2, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_action_request(EC, ActReq, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_request(EC, 2, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:encode_action_request(EC, ActReq, AsnMod, TransMod, io_list);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_request([{version3,v3}|EC], 3, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_action_request(EC, ActReq, AsnMod, TransMod, io_list);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_request(EC, 3, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:encode_action_request(EC, ActReq, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a action reply into a deep io list
%% Not yest supported by this binary codec!
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_action_reply(_EC, _V, _AcionReply) ->
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Detect (check) which version a message is
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------

%% <BACKWARD-COMPAT-CLAUSE>
version_of([{version3,v3}|EC], Binary) ->
    Decoders = [megaco_ber_media_gateway_control_v1,
		megaco_ber_media_gateway_control_v2,
		megaco_ber_media_gateway_control_v3],
    ?BIN_LIB:version_of(EC, Binary, dynamic, Decoders);
%% </BACKWARD-COMPAT-CLAUSE>

version_of(EC, Binary) ->
    Decoders = [megaco_ber_media_gateway_control_v1,
		megaco_ber_media_gateway_control_v2,
		megaco_ber_media_gateway_control_v3],
    ?BIN_LIB:version_of(EC, Binary, dynamic, Decoders).

	    
%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(EC, Binary) ->
    decode_message(EC, 1, Binary).

%% <BACKWARD-COMPAT-CLAUSE>
decode_message([{version3,v3}|EC], dynamic, Binary) ->
    Decoders = [{megaco_ber_media_gateway_control_v1,
		 megaco_binary_transformer_v1},
		{megaco_ber_media_gateway_control_v2,
		 megaco_binary_transformer_v2},
		{megaco_ber_media_gateway_control_v3,
		 megaco_binary_transformer_v3}],
    ?BIN_LIB:decode_message_dynamic(EC, Binary, Decoders, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_message(EC, dynamic, Binary) ->
    Decoders = [{megaco_ber_media_gateway_control_v1,
		 megaco_binary_transformer_v1},
		{megaco_ber_media_gateway_control_v2,
		 megaco_binary_transformer_v2},
		{megaco_ber_media_gateway_control_v3,
		 megaco_binary_transformer_v3}],
    ?BIN_LIB:decode_message_dynamic(EC, Binary, Decoders, binary);


%% -- Version 1 --
 
%% <BACKWARD-COMPAT-CLAUSE>
decode_message([{version3,_}|EC], 1, Binary) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_message(EC, 1, Binary) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);


%% -- Version 2 --
 
%% <BACKWARD-COMPAT-CLAUSE>
decode_message([{version3,_}|EC], 2, Binary) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_message(EC, 2, Binary) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);


%% -- Version 3 --
 
%% <BACKWARD-COMPAT-CLAUSE>
decode_message([{version3,v3}|EC], 3, Binary) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_message(EC, 3, Binary) ->
    AsnMod   = megaco_ber_media_gateway_control_v3,
    TransMod = megaco_binary_transformer_v3,
    ?BIN_LIB:decode_message(EC, Binary, AsnMod, TransMod, binary).


%% <BACKWARD-COMPAT-CLAUSE>
decode_mini_message([{version3,v3}|EC], dynamic, Bin) ->
    Mods = [megaco_ber_media_gateway_control_v1,
	    megaco_ber_media_gateway_control_v2,
	    megaco_ber_media_gateway_control_v3],
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_mini_message(EC, dynamic, Bin) ->
    Mods = [megaco_ber_media_gateway_control_v1,
	    megaco_ber_media_gateway_control_v2,
	    megaco_ber_media_gateway_control_v3],
    ?BIN_LIB:decode_mini_message_dynamic(EC, Bin, Mods, binary);

%% <BACKWARD-COMPAT-CLAUSE>
decode_mini_message([{version3,_}|EC], 1, Bin) ->
    AsnMod = megaco_ber_media_gateway_control_v1,
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_mini_message(EC, 1, Bin) ->
    AsnMod = megaco_ber_media_gateway_control_v1,
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);

%% <BACKWARD-COMPAT-CLAUSE>
decode_mini_message([{version3,_}|EC], 2, Bin) ->
    AsnMod = megaco_ber_media_gateway_control_v2,
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_mini_message(EC, 2, Bin) ->
    AsnMod = megaco_ber_media_gateway_control_v2,
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);

%% <BACKWARD-COMPAT-CLAUSE>
decode_mini_message([{version3,v3}|EC], 3, Bin) ->
    AsnMod = megaco_ber_media_gateway_control_v3,
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary);
%% </BACKWARD-COMPAT-CLAUSE>

decode_mini_message(EC, 3, Bin) ->
    AsnMod = megaco_ber_media_gateway_control_v3,
    ?BIN_LIB:decode_mini_message(EC, Bin, AsnMod, binary).



