%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2015. All Rights Reserved.
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
%% Purpose: Handle TLS/SSL record protocol. (Parts that are not shared with DTLS)
%%----------------------------------------------------------------------

-module(tls_record).

-include("tls_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("tls_handshake.hrl").
-include("ssl_cipher.hrl").

%% Handling of incoming data
-export([get_tls_records/2]).

%% Decoding
-export([decode_cipher_text/3]).

%% Encoding
-export([encode_plain_text/4]).

%% Protocol version handling
-export([protocol_version/1,  lowest_protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, highest_protocol_version/2,
	 is_higher/2, supported_protocol_versions/0,
	 is_acceptable_version/1, is_acceptable_version/2]).

-export_type([tls_version/0, tls_atom_version/0]).

-type tls_version()       :: ssl_record:ssl_version().
-type tls_atom_version()  :: sslv3 | tlsv1 | 'tlsv1.1' | 'tlsv1.2'.

-compile(inline).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
-spec get_tls_records(binary(), binary()) -> {[binary()], binary()} | #alert{}.
%%			     
%% Description: Given old buffer and new data from TCP, packs up a records
%% and returns it as a list of tls_compressed binaries also returns leftover
%% data
%%--------------------------------------------------------------------
get_tls_records(Data, <<>>) ->
    get_tls_records_aux(Data, []);
get_tls_records(Data, Buffer) ->
    get_tls_records_aux(list_to_binary([Buffer, Data]), []).

get_tls_records_aux(<<?BYTE(?APPLICATION_DATA),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), Data:Length/binary, Rest/binary>>, 
		    Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?APPLICATION_DATA,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
get_tls_records_aux(<<?BYTE(?HANDSHAKE),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), 
		     Data:Length/binary, Rest/binary>>, Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?HANDSHAKE,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
get_tls_records_aux(<<?BYTE(?ALERT),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), Data:Length/binary, 
		     Rest/binary>>, Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?ALERT,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
get_tls_records_aux(<<?BYTE(?CHANGE_CIPHER_SPEC),?BYTE(MajVer),?BYTE(MinVer),
		     ?UINT16(Length), Data:Length/binary, Rest/binary>>, 
		    Acc) ->
    get_tls_records_aux(Rest, [#ssl_tls{type = ?CHANGE_CIPHER_SPEC,
					version = {MajVer, MinVer},
					fragment = Data} | Acc]);
%% Matches an ssl v2 client hello message.
%% The server must be able to receive such messages, from clients that
%% are willing to use ssl v3 or higher, but have ssl v2 compatibility.
get_tls_records_aux(<<1:1, Length0:15, Data0:Length0/binary, Rest/binary>>,
		    Acc) ->
    case Data0 of
	<<?BYTE(?CLIENT_HELLO), ?BYTE(MajVer), ?BYTE(MinVer), _/binary>> ->
	    Length = Length0-1,
	    <<?BYTE(_), Data1:Length/binary>> = Data0,
	    Data = <<?BYTE(?CLIENT_HELLO), ?UINT24(Length), Data1/binary>>,
	    get_tls_records_aux(Rest, [#ssl_tls{type = ?HANDSHAKE,
						version = {MajVer, MinVer},
						fragment = Data} | Acc]);
	_ ->
	    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE)
	    
    end;

get_tls_records_aux(<<0:1, _CT:7, ?BYTE(_MajVer), ?BYTE(_MinVer),
                     ?UINT16(Length), _/binary>>,
                    _Acc) when Length > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_tls_records_aux(<<1:1, Length0:15, _/binary>>,_Acc) 
  when Length0 > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_tls_records_aux(Data, Acc) ->
    case size(Data) =< ?MAX_CIPHER_TEXT_LENGTH + ?INITIAL_BYTES of
	true ->
	    {lists:reverse(Acc), Data};
	false ->
	    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE)
	end.

encode_plain_text(Type, Version, Data,
		  #connection_states{current_write =
					 #connection_state{
					    sequence_number = Seq,
					    compression_state=CompS0,
					    security_parameters=
						#security_parameters{
						   cipher_type = ?AEAD,
						   compression_algorithm=CompAlg}
					   }= WriteState0} = ConnectionStates) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    WriteState1 = WriteState0#connection_state{compression_state = CompS1},
    AAD = calc_aad(Type, Version, WriteState1),
    {CipherFragment, WriteState} = ssl_record:cipher_aead(Version, Comp, WriteState1, AAD),
    CipherText = encode_tls_cipher_text(Type, Version, CipherFragment),
    {CipherText, ConnectionStates#connection_states{current_write = WriteState#connection_state{sequence_number = Seq +1}}};

encode_plain_text(Type, Version, Data,
		  #connection_states{current_write =
					 #connection_state{
					    sequence_number = Seq,
					    compression_state=CompS0,
					    security_parameters=
						#security_parameters{compression_algorithm=CompAlg}
					   }= WriteState0} = ConnectionStates) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    WriteState1 = WriteState0#connection_state{compression_state = CompS1},
    MacHash = calc_mac_hash(Type, Version, Comp, WriteState1),
    {CipherFragment, WriteState} = ssl_record:cipher(Version, Comp, WriteState1, MacHash),
    CipherText = encode_tls_cipher_text(Type, Version, CipherFragment),
    {CipherText, ConnectionStates#connection_states{current_write = WriteState#connection_state{sequence_number = Seq +1}}}.

%%--------------------------------------------------------------------
-spec decode_cipher_text(#ssl_tls{}, #connection_states{}, boolean()) ->
				{#ssl_tls{}, #connection_states{}}| #alert{}.
%%
%% Description: Decode cipher text
%%--------------------------------------------------------------------
decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    fragment = CipherFragment} = CipherText,
		   #connection_states{current_read =
					  #connection_state{
					     compression_state = CompressionS0,
					     sequence_number = Seq,
					     security_parameters=
						 #security_parameters{
						    cipher_type = ?AEAD,
						    compression_algorithm=CompAlg}
					    } = ReadState0} = ConnnectionStates0, _) ->
    AAD = calc_aad(Type, Version, ReadState0),
    case ssl_record:decipher_aead(Version, CipherFragment, ReadState0, AAD) of
	{PlainFragment, ReadState1} ->
	    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    ConnnectionStates = ConnnectionStates0#connection_states{
				  current_read = ReadState1#connection_state{
						   sequence_number = Seq + 1,
						   compression_state = CompressionS1}},
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	#alert{} = Alert ->
	    Alert
    end;

decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    fragment = CipherFragment} = CipherText,
		   #connection_states{current_read =
					  #connection_state{
					     compression_state = CompressionS0,
					     sequence_number = Seq,
					     security_parameters=
						 #security_parameters{compression_algorithm=CompAlg}
					    } = ReadState0} = ConnnectionStates0, PaddingCheck) ->
    case ssl_record:decipher(Version, CipherFragment, ReadState0, PaddingCheck) of
	{PlainFragment, Mac, ReadState1} ->
	    MacHash = calc_mac_hash(Type, Version, PlainFragment, ReadState1),
	    case ssl_record:is_correct_mac(Mac, MacHash) of
		true ->
		    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
								   PlainFragment, CompressionS0),
		    ConnnectionStates = ConnnectionStates0#connection_states{
					  current_read = ReadState1#connection_state{
							   sequence_number = Seq + 1,
							   compression_state = CompressionS1}},
		    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
		false ->
			?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
	    end;
	    #alert{} = Alert ->
	    Alert
    end. 
%%--------------------------------------------------------------------
-spec protocol_version(tls_atom_version() | tls_version()) -> 
			      tls_version() | tls_atom_version().		      
%%     
%% Description: Creates a protocol version record from a version atom
%% or vice versa.
%%--------------------------------------------------------------------
protocol_version('tlsv1.2') ->
    {3, 3};
protocol_version('tlsv1.1') ->
    {3, 2};
protocol_version(tlsv1) ->
    {3, 1};
protocol_version(sslv3) ->
    {3, 0};
protocol_version(sslv2) -> %% Backwards compatibility
    {2, 0};
protocol_version({3, 3}) ->
    'tlsv1.2';
protocol_version({3, 2}) ->
    'tlsv1.1';
protocol_version({3, 1}) ->
    tlsv1;
protocol_version({3, 0}) ->
    sslv3.
%%--------------------------------------------------------------------
-spec lowest_protocol_version(tls_version(), tls_version()) -> tls_version().
%%     
%% Description: Lowes protocol version of two given versions 
%%--------------------------------------------------------------------
lowest_protocol_version(Version = {M, N}, {M, O})   when N < O ->
    Version;
lowest_protocol_version({M, _}, 
			Version = {M, _}) ->
    Version;
lowest_protocol_version(Version = {M,_}, 
			{N, _}) when M < N ->
    Version;
lowest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec lowest_protocol_version([tls_version()]) -> tls_version().
%%     
%% Description: Lowest protocol version present in a list
%%--------------------------------------------------------------------
lowest_protocol_version([]) ->
    lowest_protocol_version();
lowest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    lowest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version([tls_version()]) -> tls_version().
%%     
%% Description: Highest protocol version present in a list
%%--------------------------------------------------------------------
highest_protocol_version([]) ->
    highest_protocol_version();
highest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    highest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version(tls_version(), tls_version()) -> tls_version().
%%     
%% Description: Highest protocol version of two given versions 
%%--------------------------------------------------------------------
highest_protocol_version(Version = {M, N}, {M, O})   when N > O ->
    Version;
highest_protocol_version({M, _}, 
			Version = {M, _}) ->
    Version;
highest_protocol_version(Version = {M,_}, 
			{N, _}) when M > N ->
    Version;
highest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec is_higher(V1 :: tls_version(), V2::tls_version()) -> boolean().
%%     
%% Description: Is V1 > V2
%%--------------------------------------------------------------------
is_higher({M, N}, {M, O}) when N > O ->
    true;
is_higher({M, _}, {N, _}) when M > N ->
    true; 
is_higher(_, _) ->
    false.

%%--------------------------------------------------------------------
-spec supported_protocol_versions() -> [tls_version()].					 
%%
%% Description: Protocol versions supported
%%--------------------------------------------------------------------
supported_protocol_versions() ->
    Fun = fun(Version) ->
		  protocol_version(Version) 
	  end,
    case application:get_env(ssl, protocol_version) of
	undefined ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, []} ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, Vsns} when is_list(Vsns) ->
	    Versions = lists:filter(fun is_acceptable_version/1, lists:map(Fun, Vsns)),
	    supported_protocol_versions(Versions);
	{ok, Vsn} ->
	    Versions = lists:filter(fun is_acceptable_version/1, [Fun(Vsn)]),
	    supported_protocol_versions(Versions)
    end.

supported_protocol_versions([]) ->
    Vsns = case sufficient_tlsv1_2_crypto_support() of
	       true ->
		   ?ALL_SUPPORTED_VERSIONS;
	       false ->
		   ?MIN_SUPPORTED_VERSIONS
	   end,
    application:set_env(ssl, protocol_version, Vsns),
    Vsns;

supported_protocol_versions([_|_] = Vsns) ->
    case sufficient_tlsv1_2_crypto_support() of
	true -> 
	    Vsns;
	false ->
	    case Vsns -- ['tlsv1.2'] of
		[] ->
		    ?MIN_SUPPORTED_VERSIONS;
		NewVsns ->
		    NewVsns
	    end
    end.
%%--------------------------------------------------------------------
%%     
%% Description: ssl version 2 is not acceptable security risks are too big.
%% 
%%--------------------------------------------------------------------
-spec is_acceptable_version(tls_version()) -> boolean().
is_acceptable_version({N,_}) 
  when N >= ?LOWEST_MAJOR_SUPPORTED_VERSION ->
    true;
is_acceptable_version(_) ->
    false.

-spec is_acceptable_version(tls_version(), Supported :: [tls_version()]) -> boolean().
is_acceptable_version({N,_} = Version, Versions)   
  when N >= ?LOWEST_MAJOR_SUPPORTED_VERSION ->
    lists:member(Version, Versions);
is_acceptable_version(_,_) ->
    false.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

lowest_list_protocol_version(Ver, []) ->
    Ver;
lowest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    lowest_list_protocol_version(lowest_protocol_version(Ver1, Ver2), Rest).

highest_list_protocol_version(Ver, []) ->
    Ver;
highest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    highest_list_protocol_version(highest_protocol_version(Ver1, Ver2), Rest).

encode_tls_cipher_text(Type, {MajVer, MinVer}, Fragment) ->
    Length = erlang:iolist_size(Fragment),
    [<<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Length)>>, Fragment].


mac_hash({_,_}, ?NULL, _MacSecret, _SeqNo, _Type,
	 _Length, _Fragment) ->
    <<>>;
mac_hash({3, 0}, MacAlg, MacSecret, SeqNo, Type, Length, Fragment) ->
    ssl_v3:mac_hash(MacAlg, MacSecret, SeqNo, Type, Length, Fragment);
mac_hash({3, N} = Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment)  
  when N =:= 1; N =:= 2; N =:= 3 ->
    tls_v1:mac_hash(MacAlg, MacSecret, SeqNo, Type, Version,
		      Length, Fragment).

highest_protocol_version() ->
    highest_protocol_version(supported_protocol_versions()).

lowest_protocol_version() ->
    lowest_protocol_version(supported_protocol_versions()).


sufficient_tlsv1_2_crypto_support() ->
    CryptoSupport = crypto:supports(),
    proplists:get_bool(sha256, proplists:get_value(hashs, CryptoSupport)).

calc_mac_hash(Type, Version,
	      PlainFragment, #connection_state{sequence_number = SeqNo,
					       mac_secret = MacSecret,
					       security_parameters =
						   SecPars}) ->
    Length = erlang:iolist_size(PlainFragment),
    mac_hash(Version, SecPars#security_parameters.mac_algorithm,
	     MacSecret, SeqNo, Type,
	     Length, PlainFragment).

calc_aad(Type, {MajVer, MinVer},
	 #connection_state{sequence_number = SeqNo}) ->
    <<SeqNo:64/integer, ?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer)>>.
