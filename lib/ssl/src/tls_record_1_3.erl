%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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

-module(tls_record_1_3).

-include("tls_record.hrl").
-include("tls_record_1_3.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_cipher.hrl").

%% Encoding 
-export([encode_handshake/2, encode_alert_record/2,
	 encode_data/2]).
-export([encode_plain_text/3]).

%% Decoding
-export([decode_cipher_text/2]).

%%====================================================================
%% Encoding
%%====================================================================

%%--------------------------------------------------------------------
-spec encode_handshake(iolist(), ssl_record:connection_states()) ->
			      {iolist(), ssl_record:connection_states()}.
%
%% Description: Encodes a handshake message to send on the tls-1.3-socket.
%%--------------------------------------------------------------------
encode_handshake(Frag, ConnectionStates) ->
    case iolist_size(Frag) of
	N  when N > ?MAX_PLAIN_TEXT_LENGTH ->
            %% TODO: Consider padding here
	    Data = split_bin(iolist_to_binary(Frag), ?MAX_PLAIN_TEXT_LENGTH),
	    encode_iolist(?HANDSHAKE, Data, ConnectionStates);
	_  ->
	    encode_plain_text(?HANDSHAKE, Frag, ConnectionStates)
    end.

%%--------------------------------------------------------------------
-spec encode_alert_record(#alert{}, ssl_record:connection_states()) ->
				 {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes an alert message to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_alert_record(#alert{level = Level, description = Description},
                    ConnectionStates) ->
    encode_plain_text(?ALERT, <<?BYTE(Level), ?BYTE(Description)>>,
		      ConnectionStates).
%%--------------------------------------------------------------------
-spec encode_data(binary(), ssl_record:connection_states()) ->
			 {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes data to send on the ssl-socket.
%%--------------------------------------------------------------------
encode_data(Frag, ConnectionStates) ->
    Data = split_bin(Frag, ?MAX_PLAIN_TEXT_LENGTH, {3,4}),
    encode_iolist(?APPLICATION_DATA, Data, ConnectionStates).

encode_plain_text(Type, Data0, #{current_write := Write0} = ConnectionStates) ->
    PadLen = 0, %% TODO where to specify PadLen?
    Data = inner_plaintext(Type, Data0, PadLen),
    {CipherFragment, Write1} = encode_plain_text(Data, Write0),
    {CipherText, Write} = encode_tls_cipher_text(CipherFragment, Write1),
    {CipherText, ConnectionStates#{current_write => Write}}.

encode_iolist(Type, Data, ConnectionStates0) ->
    {ConnectionStates, EncodedMsg} =
        lists:foldl(fun(Text, {CS0, Encoded}) ->
			    {Enc, CS1} =
				encode_plain_text(Type, Text, CS0),
			    {CS1, [Enc | Encoded]}
		    end, {ConnectionStates0, []}, Data),
    {lists:reverse(EncodedMsg), ConnectionStates}.

%%====================================================================
%% Decoding
%%====================================================================

%%--------------------------------------------------------------------
-spec decode_cipher_text(#ssl_tls{}, ssl_record:connection_states()) ->
				{#ssl_tls{}, ssl_record:connection_states()}| #alert{}.
%%
%% Description: Decode cipher text, use legacy type ssl_tls instead of tls_cipher_text
%% in decoding context so that we can reuse the code from erlier versions. 
%%--------------------------------------------------------------------
decode_cipher_text(#ssl_tls{type = ?OPAQUE_TYPE,
                            version = ?LEGACY_VERSION,
                            fragment = CipherFragment},
		   #{current_read :=
			 #{sequence_number := Seq,
                           cipher_state := CipherS0,
			   security_parameters :=
			       #security_parameters{
				  cipher_type = ?AEAD,
                                  bulk_cipher_algorithm =
                                      BulkCipherAlgo}
			  } = ReadState0} = ConnnectionStates0) ->
    AAD = start_additional_data(),
    CipherS1 = ssl_cipher:nonce_seed(<<?UINT64(Seq)>>, CipherS0),
    case decipher_aead(BulkCipherAlgo, CipherS1, AAD, CipherFragment) of
	{PlainFragment, CipherS1} ->
	    ConnnectionStates = 
                ConnnectionStates0#{current_read => 
                                        ReadState0#{cipher_state => CipherS1,
                                                    sequence_number => Seq + 1}},
	    decode_inner_plaintext(PlainFragment, ConnnectionStates);
	#alert{} = Alert ->
	    Alert
    end;
decode_cipher_text(#ssl_tls{type = Type}, _) ->
    %% Version mismatch is already asserted  
    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, {record_typ_mismatch, Type}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
split_bin(Bin, ChunkSize) ->
    split_bin(Bin, ChunkSize, []).
split_bin(Bin, ChunkSize, _) ->
    do_split_bin(Bin, ChunkSize, []).

do_split_bin(<<>>, _, Acc) ->
    lists:reverse(Acc);
do_split_bin(Bin, ChunkSize, Acc) ->
    case Bin of
        <<Chunk:ChunkSize/binary, Rest/binary>> ->
            do_split_bin(Rest, ChunkSize, [Chunk | Acc]);
        _ ->
            lists:reverse(Acc, [Bin])
    end.

inner_plaintext(Type, Data, Length) ->
    #inner_plaintext{
       content = Data,
       type = Type,
       zeros = zero_padding(Length)
      }.
zero_padding(Length)->
    binary:copy(<<?BYTE(0)>>, Length).

encode_plain_text(#inner_plaintext{
                     content = Data,
                     type = Type,
                     zeros = Zeros
                    }, #{cipher_state := CipherS0,
                         sequence_number := Seq,
                         security_parameters :=
                             #security_parameters{
                                cipher_type = ?AEAD}
                        } = WriteState0) ->
    PlainText = <<Data/binary, ?BYTE(Type), Zeros/binary>>,
    AAD = start_additional_data(),
    CipherS1 = ssl_cipher:nonce_seed(<<?UINT64(Seq)>>, CipherS0),
    {Encoded, WriteState} = cipher_aead(PlainText, WriteState0#{cipher_state => CipherS1}, AAD),
    {#tls_cipher_text{encoded_record = Encoded}, WriteState};
encode_plain_text(_, CS) ->
    exit({cs, CS}).

start_additional_data() ->
    {MajVer, MinVer} = ?LEGACY_VERSION,
    <<?BYTE(?OPAQUE_TYPE), ?BYTE(MajVer), ?BYTE(MinVer)>>.

end_additional_data(AAD, Len) ->
    <<AAD/binary, ?UINT16(Len)>>.

nonce(#cipher_state{nonce = Nonce, iv = IV}) ->
    Len = size(IV),
    crypto:exor(<<Nonce:Len/bytes>>, IV).

cipher_aead(Fragment,
	    #{cipher_state := CipherS0,
	      security_parameters :=
		  #security_parameters{bulk_cipher_algorithm =
					   BulkCipherAlgo}
	     } = WriteState0, AAD) ->
    {CipherFragment, CipherS1} =
	cipher_aead(BulkCipherAlgo, CipherS0, AAD, Fragment),
    {CipherFragment,  WriteState0#{cipher_state => CipherS1}}.

cipher_aead(Type, #cipher_state{key=Key} = CipherState, AAD0, Fragment) ->
    AAD = end_additional_data(AAD0, erlang:iolist_size(Fragment)),
    Nonce = nonce(CipherState),
    {Content, CipherTag} = ssl_cipher:aead_encrypt(Type, Key, Nonce, Fragment, AAD),
    {<<Content/binary, CipherTag/binary>>, CipherState}.

encode_tls_cipher_text(#tls_cipher_text{opaque_type = Type,
                                        legacy_version = {MajVer, MinVer},
                                        encoded_record = Encoded}, #{sequence_number := Seq} = Write) ->
    Length = erlang:iolist_size(Encoded),
    {[<<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Length)>>, Encoded],
     Write#{sequence_number => Seq +1}}.

decipher_aead(Type, #cipher_state{key = Key} = CipherState, AAD0, CipherFragment) ->
    try
        Nonce = nonce(CipherState),
        {AAD, CipherText, CipherTag} = aead_ciphertext_split(CipherState, CipherFragment, AAD0),
	case ssl_cipher:aead_decrypt(Type, Key, Nonce, CipherText, CipherTag, AAD) of
	    Content when is_binary(Content) ->
		{Content, CipherState};
	    _ ->
                ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
	end
    catch
	_:_ ->
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
    end.

aead_ciphertext_split(#cipher_state{tag_len = Len}, CipherTextFragment, AAD) ->
    CipherLen = size(CipherTextFragment) - Len,
    <<CipherText:CipherLen/bytes, CipherTag:Len/bytes>> = CipherTextFragment,
    {end_additional_data(AAD, CipherLen), CipherText, CipherTag}.

decode_inner_plaintext(PlainText, ConnnectionStates) ->
    case remove_padding(PlainText) of
        #alert{} = Alert ->
            Alert;
        {Data, Type} ->
            {#ssl_tls{type = Type,
                      version = {3,4}, %% Internally use real version
                      fragment = Data}, ConnnectionStates}
    end.

remove_padding(PlainText)->
    case binary:split(PlainText, <<0>>,  [global, trim]) of
        [] ->
            ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, padding_error);
        [Content] ->
            Type = binary:last(Content),
            split_content(Type, Content, erlang:byte_size(Content) - 1)
    end.

split_content(?HANDSHAKE, _, 0) ->
    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, empty_handshake);
split_content(?ALERT, _, 0) ->
    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, empty_alert);
%% For special middlebox compatible case!
split_content(?CHANGE_CIPHER_SPEC, _, 0) ->
    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, empty_change_cipher_spec);
split_content(?APPLICATION_DATA = Type, _, 0) ->
    {Type, <<>>};
split_content(Type, Content, N) ->
    <<Data:N/bytes, ?BYTE(Type)>> = Content, 
    {Type, Data}.
