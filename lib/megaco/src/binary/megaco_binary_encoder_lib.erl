%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(megaco_binary_encoder_lib).

%% API
-export([
	 version_of/4, 
	 decode_message/5, decode_message_dynamic/4, 
	 decode_mini_message/4, decode_mini_message_dynamic/4, 
	 encode_message/5, 
	 encode_transaction/5, 
	 encode_action_requests/5, 
	 encode_action_request/5,
	 encode_action_reply/5
	]).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% Detect (check) which version a message is
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------

version_of(_EC, Binary, dynamic, [AsnModV1|_AsnMods]) 
  when is_binary(Binary) andalso is_atom(AsnModV1) ->
    case (catch AsnModV1:decode_message_version(Binary)) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Error ->
	    Error
    end;
version_of(_EC, Binary, 1, AsnMods) 
  when is_binary(Binary) andalso is_list(AsnMods) ->
    version_of(AsnMods, Binary, []);
version_of(_EC, Binary, 2, [AsnModV1, AsnModV2, AsnModV3]) 
  when is_binary(Binary) ->
    version_of([AsnModV2, AsnModV1, AsnModV3], Binary, []);
version_of(_EC, Binary, 3, [AsnModV1, AsnModV2, AsnModV3]) 
  when is_binary(Binary) ->
    version_of([AsnModV3, AsnModV1, AsnModV2], Binary, []).

version_of([], _Binary, Err) ->
    {error, {decode_failed, lists:reverse(Err)}};
version_of([AsnMod|AsnMods], Binary, Errs) when is_atom(AsnMod) ->
    case (catch AsnMod:decode('MegacoMessage', Binary)) of
	{ok, M} ->
	    V = (M#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Err ->
	    version_of(AsnMods, Binary, [Err|Errs])
    end.

	    
%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message([native], MegaMsg, AsnMod, _TransMod, binary) 
  when is_record(MegaMsg, 'MegacoMessage') ->
    AsnMod:encode('MegacoMessage', MegaMsg);
encode_message(EC, MegaMsg, AsnMod, TransMod, binary) 
  when is_list(EC) andalso is_record(MegaMsg, 'MegacoMessage') ->
    case (catch TransMod:tr_message(MegaMsg, encode, EC)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	MegaMsg2 ->
	    AsnMod:encode('MegacoMessage', MegaMsg2)
    end;
encode_message(EC, MegaMsg, AsnMod, TransMod, io_list) ->
    case encode_message(EC, MegaMsg, AsnMod, TransMod, binary) of
	{ok, Bin} when is_binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_message(EC, MegaMsg, _AsnMod, _TransMod, _Type)
  when is_record(MegaMsg, 'MegacoMessage')  ->
    {error, {bad_encoding_config, EC}};
encode_message(_EC, MegaMsg, _AsnMod, _TransMod, _Type) ->
    {error, {no_megaco_message, MegaMsg}}.


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

%% Should handle encoding of all types of transactions:
%% TransactionAck, TransactionPending, TransactionRequest
%% and TransactionReply
encode_transaction(EC, {Tag, _} = Trans, AsnMod, TransMod, Type) 
  when (Tag == transactionResponseAck) ->
    do_encode_transaction(EC, Trans, AsnMod, TransMod, Type);
encode_transaction(EC, {Tag, _} = Trans, AsnMod, TransMod, Type) 
  when (Tag == transactionPending) -> 
    do_encode_transaction(EC, Trans, AsnMod, TransMod, Type);
encode_transaction(EC, {Tag, _} = Trans, AsnMod, TransMod, Type) 
  when (Tag == transactionRequest) ->
    do_encode_transaction(EC, Trans, AsnMod, TransMod, Type);
%% TransactionReply has been changed as of v3 so we cannot use 
%% the record definition in this common module.
encode_transaction(EC, {Tag, _} = Trans, AsnMod, TransMod, Type) 
  when (Tag == transactionReply) ->
    do_encode_transaction(EC, Trans, AsnMod, TransMod, Type);
encode_transaction(_EC, T, _AsnMod, _TransMod, _Type) ->
    {error, {no_megaco_transaction, T}}.

-spec do_encode_transaction(EC :: list(), 
			    Trans :: tuple(), 
			    AnsMod :: atom(),
			    TransMod :: atom(),
			    Type :: atom()) ->
    {'ok', binary()} | {'error', any()}.
do_encode_transaction([native], _Trans, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
do_encode_transaction(EC, _Trans, _AsnMod, _TransMod, binary) 
  when is_list(EC) ->
    %% T2 = TransMod:tr_transaction(Trans, encode, EC),
    %% asn1rt:encode(AsnMod, element(1, T), T2);
    {error, not_implemented};
do_encode_transaction(EC, Trans, AsnMod, TransMod, io_list) ->
    case do_encode_transaction(EC, Trans, AsnMod, TransMod, binary) of
	{ok, Bin} when is_binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
do_encode_transaction(EC, _Trans, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, EC}}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
-spec encode_action_requests(EC :: list(), 
			     ARs :: list(), 
			     AnsMod :: atom(),
			     TransMod :: atom(),
			     Type :: atom()) ->
    {'ok', binary()} | {'error', any()}.
encode_action_requests([native], _ARs, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
encode_action_requests(_EC, _ARs0, _AsnMod, _TransMod, binary) ->
    {error, not_implemented};
encode_action_requests(EC, ARs, AsnMod, TransMod, io_list) ->
    case encode_action_requests(EC, ARs, AsnMod, TransMod, binary) of
	{ok, Bin} when is_binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_action_requests(EC, _ARs, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, EC}}.


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

-spec encode_action_request(EC :: list(), 
			    AR :: tuple(), 
			    AnsMod :: atom(),
			    TransMod :: atom(),
			    Type :: atom()) ->
    {'ok', binary()} | {'error', any()}.
encode_action_request([native], _AR, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
encode_action_request(_EC, _AR, _AsnMod, _TransMod, binary) ->
    {error, not_implemented};
encode_action_request(EC, AR, AsnMod, TransMod, io_list) ->
    case encode_action_request(EC, AR, AsnMod, TransMod, binary) of
	{ok, Bin} when is_binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_action_request(EC, _AR, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, EC}}.


%%----------------------------------------------------------------------
%% Convert a ActionReply record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_reply([native], _ARs, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
encode_action_reply(_EC, _ARs0, _AsnMod, _TransMod, binary) ->
    {error, not_implemented};
encode_action_reply(EC, ARs, AsnMod, TransMod, io_list) ->
    case encode_action_reply(EC, ARs, AsnMod, TransMod, binary) of
	{ok, Bin} when is_binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_action_reply(EC, _ARs, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, EC}}.


%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message_dynamic(EC, Bin, 
		       [{AsnModV1, TransModV1}, 
			{AsnModV2, TransModV2}, 
			{AsnModV3, TransModV3}], Form)
  when is_list(EC) andalso is_binary(Bin) ->
    case AsnModV1:decode_message_version(Bin) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    case V of
		1 -> 
		    decode_message(EC, Bin, AsnModV1, TransModV1, Form);
		2 ->
		    decode_message(EC, Bin, AsnModV2, TransModV2, Form);
		3 ->
		    decode_message(EC, Bin, AsnModV3, TransModV3, Form)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_message_dynamic(EC, Bin, _Mods, _Type) 
  when is_binary(Bin) ->
    {error, {bad_encoding_config, EC}};
decode_message_dynamic(_EC, _BadBin, _Mods, _Type) ->
    {error, no_binary}.


decode_message(EC, Bin, AsnMod, TransMod, _) ->
    case AsnMod:decode('MegacoMessage', Bin) of
	{ok, MegaMsg} ->
	    case EC of
		[native] ->
		    {ok, MegaMsg};
		_ ->		
		    {ok, TransMod:tr_message(MegaMsg, decode, EC)}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.


%%----------------------------------------------------------------------
%% Convert a binary into a partial 'MegacoMessage' record
%% I.e. only version and Mid is fully decoded.
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_mini_message(_, Bin, Mod, _) ->
    case (catch Mod:decode_message_mId(Bin)) of
	{ok, #'MegacoMessage'{mess = Mess} = MegaMsg} ->
	    Mess2 = Mess#'Message'{messageBody = undefined},
	    {ok, MegaMsg#'MegacoMessage'{mess = Mess2}};
	Error ->
	    Error
    end.


decode_mini_message_dynamic(EC, Bin, [Mod1, Mod2, Mod3], Form) ->
    case Mod1:decode_message_version(Bin) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    case V of
		1 -> 
		    decode_mini_message(EC, Bin, Mod1, Form);
		2 ->
		    decode_mini_message(EC, Bin, Mod2, Form);
		3 ->
		    decode_mini_message(EC, Bin, Mod3, Form)
	    end;
	Error ->
	    Error
    end.

