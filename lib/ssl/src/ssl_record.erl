%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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
%% Purpose: Help functions for handling the SSL-Record protocol 
%% 
%%----------------------------------------------------------------------

-module(ssl_record).

-include("ssl_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_handshake.hrl").
-include("ssl_cipher.hrl").
-include("ssl_debug.hrl").

%% Connection state handling
-export([init_connection_states/1, 
         current_connection_state/2, pending_connection_state/2,
         update_security_params/3,
         set_mac_secret/4,
	 set_master_secret/2, 
         activate_pending_connection_state/2,
         set_pending_cipher_state/4,
	 set_renegotiation_flag/2,
	 set_client_verify_data/3,
	 set_server_verify_data/3]).

%% Handling of incoming data
-export([get_tls_records/2]).

%% Encoding records
-export([encode_handshake/3, encode_alert_record/3,
	 encode_change_cipher_spec/2, encode_data/4]).

%% Decoding
-export([decode_cipher_text/2]).

%% Misc.
-export([protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, supported_protocol_versions/0,
	 is_acceptable_version/1]).

-export([compressions/0]).

-compile(inline).

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init_connection_states(Role) -> #connection_states{} 
%%	Role = client | server
%%      Random = binary()
%%
%% Description: Creates a connection_states record with appropriate
%% values for the initial SSL connection setup. 
%%--------------------------------------------------------------------
init_connection_states(Role) ->
    ConnectionEnd = record_protocol_role(Role),
    Current = initial_connection_state(ConnectionEnd),
    Pending = empty_connection_state(ConnectionEnd),
    #connection_states{current_read = Current,
		       pending_read = Pending,
		       current_write = Current,
		       pending_write = Pending
                      }.

%%--------------------------------------------------------------------
%% Function: current_connection_state(States, Type) -> #connection_state{}
%%	States = #connection_states{}
%%      Type = read | write
%%
%% Description: Returns the instance of the connection_state record
%% that is currently defined as the current conection state.
%%--------------------------------------------------------------------  
current_connection_state(#connection_states{current_read = Current},
			 read) ->
    Current;
current_connection_state(#connection_states{current_write = Current},
			 write) ->
    Current.

%%--------------------------------------------------------------------
%% Function: pending_connection_state(States, Type) -> #connection_state{}
%%	States = #connection_states{}
%%      Type = read | write
%%
%% Description: Returns the instance of the connection_state record
%% that is currently defined as the pending conection state.
%%--------------------------------------------------------------------  
pending_connection_state(#connection_states{pending_read = Pending},    
			 read) ->
    Pending;
pending_connection_state(#connection_states{pending_write = Pending},
			 write) ->
    Pending.

%%--------------------------------------------------------------------
%% Function: update_security_params(Params, States) -> 
%%                                                     #connection_states{}
%%      Params = #security_parameters{}
%%	States = #connection_states{}
%%
%% Description: Creates a new instance of the connection_states record
%% where the pending states gets its security parameters
%% updated to <Params>.
%%--------------------------------------------------------------------  
update_security_params(ReadParams, WriteParams, States = 
		       #connection_states{pending_read = Read,
					  pending_write = Write}) -> 
    States#connection_states{pending_read =
                             Read#connection_state{security_parameters = 
                                                   ReadParams},
                             pending_write = 
                             Write#connection_state{security_parameters = 
                                                    WriteParams}
                            }.
%%--------------------------------------------------------------------
%% Function: set_mac_secret(ClientWriteMacSecret,
%%                            ServerWriteMacSecret, Role, States) -> 
%%                                       #connection_states{}
%%      MacSecret = binary()
%%	States = #connection_states{}
%%	Role = server | client
%%
%% update the mac_secret field in pending connection states
%%--------------------------------------------------------------------
set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, client, States) ->
    set_mac_secret(ServerWriteMacSecret, ClientWriteMacSecret, States);
set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, server, States) ->
    set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret, States).

set_mac_secret(ReadMacSecret, WriteMacSecret,
	       States = #connection_states{pending_read = Read,
					   pending_write = Write}) ->
    States#connection_states{
      pending_read = Read#connection_state{mac_secret = ReadMacSecret},
      pending_write = Write#connection_state{mac_secret = WriteMacSecret}
     }.


%%--------------------------------------------------------------------
%% Function: set_master_secret(MasterSecret, States) -> 
%%                                                #connection_states{}
%%      MacSecret = 
%%	States = #connection_states{}
%%
%% Set master_secret in pending connection states
%%--------------------------------------------------------------------
set_master_secret(MasterSecret,
                  States = #connection_states{pending_read = Read,
                                              pending_write = Write}) -> 
    ReadSecPar = Read#connection_state.security_parameters,
    Read1 = Read#connection_state{
              security_parameters = ReadSecPar#security_parameters{
                                      master_secret = MasterSecret}},
    WriteSecPar = Write#connection_state.security_parameters,
    Write1 = Write#connection_state{
               security_parameters = WriteSecPar#security_parameters{
                                       master_secret = MasterSecret}},
    States#connection_states{pending_read = Read1, pending_write = Write1}.

%%--------------------------------------------------------------------
%% Function: set_renegotiation_flag(Flag, States) -> 
%%                                                #connection_states{}
%%      Flag = boolean()
%%	States = #connection_states{}
%%
%% Set master_secret in pending connection states
%%--------------------------------------------------------------------
set_renegotiation_flag(Flag, #connection_states{
			 current_read = CurrentRead0,
			 current_write = CurrentWrite0,
			 pending_read = PendingRead0,
			 pending_write = PendingWrite0} 
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{secure_renegotiation = Flag},
    CurrentWrite = CurrentWrite0#connection_state{secure_renegotiation = Flag},
    PendingRead = PendingRead0#connection_state{secure_renegotiation = Flag},
    PendingWrite = PendingWrite0#connection_state{secure_renegotiation = Flag},
    ConnectionStates#connection_states{current_read = CurrentRead, 
				       current_write = CurrentWrite, 
				       pending_read = PendingRead, 
				       pending_write = PendingWrite}.

%%--------------------------------------------------------------------
%% Function: set_client_verify_data(State, Data, States) -> 
%%                                                #connection_states{}
%%      State = atom()
%%      Data = binary()
%%	States = #connection_states{}
%%
%% Set verify data in connection states.                 
%%--------------------------------------------------------------------
set_client_verify_data(current_read, Data, 
		       #connection_states{current_read = CurrentRead0,
					  pending_write = PendingWrite0} 
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{client_verify_data = Data},
    PendingWrite = PendingWrite0#connection_state{client_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       pending_write = PendingWrite};
set_client_verify_data(current_write, Data, 
		       #connection_states{pending_read = PendingRead0,
					  current_write = CurrentWrite0} 
		       = ConnectionStates) ->
    PendingRead = PendingRead0#connection_state{client_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{client_verify_data = Data},
    ConnectionStates#connection_states{pending_read = PendingRead,
				       current_write = CurrentWrite};
set_client_verify_data(current_both, Data, 
		       #connection_states{current_read = CurrentRead0,
					  current_write = CurrentWrite0} 
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{client_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{client_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       current_write = CurrentWrite}.

%%--------------------------------------------------------------------
%% Function: set_server_verify_data(State, Data, States) -> 
%%                                                #connection_states{}
%%      State = atom()
%%      Data = binary()
%%	States = #connection_states{}
%%
%% Set verify data in pending connection states.
%%--------------------------------------------------------------------
set_server_verify_data(current_write, Data, 
		       #connection_states{pending_read = PendingRead0,
					  current_write = CurrentWrite0} 
		       = ConnectionStates) ->
    PendingRead = PendingRead0#connection_state{server_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{server_verify_data = Data},
    ConnectionStates#connection_states{pending_read = PendingRead,
				       current_write = CurrentWrite};

set_server_verify_data(current_read, Data, 
		       #connection_states{current_read = CurrentRead0,
					  pending_write = PendingWrite0} 
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{server_verify_data = Data},
    PendingWrite = PendingWrite0#connection_state{server_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       pending_write = PendingWrite};

set_server_verify_data(current_both, Data, 
		       #connection_states{current_read = CurrentRead0,
					  current_write = CurrentWrite0} 
		       = ConnectionStates) ->
    CurrentRead = CurrentRead0#connection_state{server_verify_data = Data},
    CurrentWrite = CurrentWrite0#connection_state{server_verify_data = Data},
    ConnectionStates#connection_states{current_read = CurrentRead,
				       current_write = CurrentWrite}.

%%--------------------------------------------------------------------
%% Function: activate_pending_connection_state(States, Type) -> 
%%                                                   #connection_states{} 
%%	States = #connection_states{}
%%      Type = read | write
%%
%% Description: Creates a new instance of the connection_states record
%% where the pending state of <Type> has been activated. 
%%--------------------------------------------------------------------
activate_pending_connection_state(States = 
                                  #connection_states{pending_read = Pending},
                                  read) ->
    NewCurrent = Pending#connection_state{sequence_number = 0},
    SecParams = Pending#connection_state.security_parameters,
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = empty_connection_state(ConnectionEnd),
    SecureRenegotation = NewCurrent#connection_state.secure_renegotiation,
    NewPending = EmptyPending#connection_state{secure_renegotiation = SecureRenegotation},
    States#connection_states{current_read = NewCurrent,
                             pending_read = NewPending
                            };

activate_pending_connection_state(States = 
                                  #connection_states{pending_write = Pending},
                                  write) ->
    NewCurrent = Pending#connection_state{sequence_number = 0},
    SecParams = Pending#connection_state.security_parameters,
    ConnectionEnd = SecParams#security_parameters.connection_end,
    EmptyPending = empty_connection_state(ConnectionEnd),
    SecureRenegotation = NewCurrent#connection_state.secure_renegotiation,
    NewPending = EmptyPending#connection_state{secure_renegotiation = SecureRenegotation},
    States#connection_states{current_write = NewCurrent,
                             pending_write = NewPending
                            }.

%%--------------------------------------------------------------------
%% Function: set_pending_cipher_state(States, ClientState, 
%%                                    ServerState, Role) -> 
%%                                                #connection_states{}
%%       ClientState = ServerState = #cipher_state{}
%%	 States = #connection_states{}
%%
%% Description: Set the cipher state in the specified pending connection state.
%%--------------------------------------------------------------------
set_pending_cipher_state(#connection_states{pending_read = Read,
                                            pending_write = Write} = States,
                         ClientState, ServerState, server) ->
    States#connection_states{
        pending_read = Read#connection_state{cipher_state = ClientState},
        pending_write = Write#connection_state{cipher_state = ServerState}};

set_pending_cipher_state(#connection_states{pending_read = Read,
                                            pending_write = Write} = States,
                         ClientState, ServerState, client) ->
    States#connection_states{
        pending_read = Read#connection_state{cipher_state = ServerState},
        pending_write = Write#connection_state{cipher_state = ClientState}}.

%%--------------------------------------------------------------------
%% Function: get_tls_record(Data, Buffer) -> Result
%%      Result = {[#tls_compressed{}], NewBuffer}
%%      Data = Buffer = NewBuffer = binary()
%%
%% Description: given old buffer and new data from TCP, packs up a records
%% and returns it as a list of #tls_compressed, also returns leftover
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
%% Matches a ssl v2 client hello message.
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
                    _Acc) when Length > ?MAX_CIPHER_TEXT_LENGTH->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_tls_records_aux(<<1:1, Length0:15, _/binary>>,_Acc) 
  when Length0 > ?MAX_CIPHER_TEXT_LENGTH->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_tls_records_aux(Data, Acc) ->
    {lists:reverse(Acc), Data}.

%%--------------------------------------------------------------------
%% Function: protocol_version(Version) -> #protocol_version{}
%%     Version = atom()
%%     
%% Description: Creates a protocol version record from a version atom
%% or vice versa.
%%--------------------------------------------------------------------
protocol_version('tlsv1.1') ->
    {3, 2};
protocol_version(tlsv1) ->
    {3, 1};
protocol_version(sslv3) ->
    {3, 0};
protocol_version(sslv2) -> %% Backwards compatibility
    {2, 0};
protocol_version({3, 2}) ->
    'tlsv1.1';
protocol_version({3, 1}) ->
    tlsv1;
protocol_version({3, 0}) ->
    sslv3.
%%--------------------------------------------------------------------
%% Function: protocol_version(Version1, Version2) -> #protocol_version{}
%%     Version1 = Version2 = #protocol_version{}
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
%% Function: protocol_version(Versions) -> #protocol_version{}
%%     Versions = [#protocol_version{}]
%%     
%% Description: Highest protocol version present in a list
%%--------------------------------------------------------------------
highest_protocol_version([]) ->
    highest_protocol_version();
highest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    highest_protocol_version(Ver, Vers).

highest_protocol_version(Version, []) ->
    Version;
highest_protocol_version(Version = {N, M}, [{N, O} | Rest])   when M > O ->
    highest_protocol_version(Version, Rest);
highest_protocol_version({M, _}, [Version = {M, _} | Rest]) ->
    highest_protocol_version(Version, Rest);
highest_protocol_version(Version = {M,_}, [{N,_} | Rest])  when M > N ->
    highest_protocol_version(Version, Rest);
highest_protocol_version(_, [Version | Rest]) ->
    highest_protocol_version(Version, Rest).

%%--------------------------------------------------------------------
%% Function: supported_protocol_versions() -> Versions
%%     Versions = [#protocol_version{}]
%%     
%% Description: Protocol versions supported
%%--------------------------------------------------------------------
supported_protocol_versions() ->
    Fun = fun(Version) ->
		  protocol_version(Version) 
	  end,
    case application:get_env(ssl, protocol_version) of
	undefined ->
	    lists:map(Fun, ?DEFAULT_SUPPORTED_VERSIONS);
	{ok, []} ->
	    lists:map(Fun, ?DEFAULT_SUPPORTED_VERSIONS);
	{ok, Vsns} when is_list(Vsns) ->
	    Versions = lists:filter(fun is_acceptable_version/1, lists:map(Fun, Vsns)),
	    supported_protocol_versions(Versions);
	{ok, Vsn} ->
	    Versions = lists:filter(fun is_acceptable_version/1, [Fun(Vsn)]),
	    supported_protocol_versions(Versions)
    end.

supported_protocol_versions([]) ->
    ?DEFAULT_SUPPORTED_VERSIONS;
supported_protocol_versions([_|_] = Vsns) ->
    Vsns.

%%--------------------------------------------------------------------
%% Function: is_acceptable_version(Version) -> true | false
%%     Version = #protocol_version{}
%%     
%% Description: ssl version 2 is not acceptable security risks are too big.
%%--------------------------------------------------------------------
is_acceptable_version({N,_}) 
  when N >= ?LOWEST_MAJOR_SUPPORTED_VERSION ->
    true;
is_acceptable_version(_) ->
    false.

%%--------------------------------------------------------------------
%% Function: compressions() -> binary()
%%     
%% Description: return a list of compressions supported (currently none)
%%--------------------------------------------------------------------
compressions() ->
    [?byte(?NULL)].

%%--------------------------------------------------------------------
%% Function: decode_cipher_text(CipherText, ConnectionStates0) -> 
%%                                     {Plain, ConnectionStates}
%%     
%% Description: Decode cipher text
%%--------------------------------------------------------------------
decode_cipher_text(CipherText, ConnnectionStates0) ->
    ReadState0 = ConnnectionStates0#connection_states.current_read,
    #connection_state{compression_state = CompressionS0,
		      security_parameters = SecParams} = ReadState0,
    CompressAlg = SecParams#security_parameters.compression_algorithm,
   case decipher(CipherText, ReadState0) of
       {Compressed, ReadState1} ->
	   {Plain, CompressionS1} = uncompress(CompressAlg, 
					       Compressed, CompressionS0),
	   ConnnectionStates = ConnnectionStates0#connection_states{
				 current_read = ReadState1#connection_state{
						  compression_state = CompressionS1}},
	   {Plain, ConnnectionStates};
       #alert{} = Alert ->
	   Alert
   end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
highest_protocol_version() ->
    highest_protocol_version(supported_protocol_versions()).

initial_connection_state(ConnectionEnd) ->
    #connection_state{security_parameters =
                      initial_security_params(ConnectionEnd),
                      sequence_number = 0
                     }.

initial_security_params(ConnectionEnd) ->
    SecParams = #security_parameters{connection_end = ConnectionEnd,
				     compression_algorithm = ?NULL},
    ssl_cipher:security_parameters(?TLS_NULL_WITH_NULL_NULL, 
				   SecParams).

empty_connection_state(ConnectionEnd) ->
    SecParams = empty_security_params(ConnectionEnd),
    #connection_state{security_parameters = SecParams}.

empty_security_params(ConnectionEnd = ?CLIENT) ->
    #security_parameters{connection_end = ConnectionEnd,
                         client_random = random()};
empty_security_params(ConnectionEnd = ?SERVER) ->
    #security_parameters{connection_end = ConnectionEnd,
                         server_random = random()}.
random() ->
    Secs_since_1970 = calendar:datetime_to_gregorian_seconds(
			calendar:universal_time()) - 62167219200,
    Random_28_bytes = crypto:rand_bytes(28),
    <<?UINT32(Secs_since_1970), Random_28_bytes/binary>>.

record_protocol_role(client) ->
    ?CLIENT;
record_protocol_role(server) ->
    ?SERVER.

split_bin(Bin, ChunkSize) ->
    split_bin(Bin, ChunkSize, []).

split_bin(<<>>, _, Acc) ->
    lists:reverse(Acc);
split_bin(Bin, ChunkSize, Acc) ->
    case Bin of
        <<Chunk:ChunkSize/binary, Rest/binary>> ->
            split_bin(Rest, ChunkSize, [Chunk | Acc]);
        _ ->
            lists:reverse(Acc, [Bin])
    end.

encode_data(Frag, Version, ConnectionStates, RenegotiateAt) 
  when byte_size(Frag) < (?MAX_PLAIN_TEXT_LENGTH - 2048) -> 
    case encode_plain_text(?APPLICATION_DATA,Version,Frag,ConnectionStates, RenegotiateAt) of
	{renegotiate, Data} ->
	    {[], Data, ConnectionStates};
	{Msg, CS} ->
	    {Msg, [], CS}
    end;

encode_data(Frag, Version, ConnectionStates, RenegotiateAt) when is_binary(Frag) ->
    Data = split_bin(Frag, ?MAX_PLAIN_TEXT_LENGTH - 2048),
    encode_data(Data, Version, ConnectionStates, RenegotiateAt);

encode_data(Data, Version, ConnectionStates0, RenegotiateAt) when is_list(Data) ->
    {ConnectionStates, EncodedMsg, NotEncdedData} = 
        lists:foldl(fun(B, {CS0, Encoded, Rest}) ->
			    case encode_plain_text(?APPLICATION_DATA,
						   Version, B, CS0, RenegotiateAt) of
				{renegotiate, NotEnc} ->
				    {CS0, Encoded, [NotEnc | Rest]};
				{Enc, CS1} ->
				    {CS1, [Enc | Encoded], Rest}
			    end 
		    end, {ConnectionStates0, [], []}, Data),
    {lists:reverse(EncodedMsg), lists:reverse(NotEncdedData), ConnectionStates}.

encode_handshake(Frag, Version, ConnectionStates) ->
    encode_plain_text(?HANDSHAKE, Version, Frag, ConnectionStates).

encode_alert_record(#alert{level = Level, description = Description},
                    Version, ConnectionStates) ->
    encode_plain_text(?ALERT, Version, <<?BYTE(Level), ?BYTE(Description)>>, 
		      ConnectionStates).

encode_change_cipher_spec(Version, ConnectionStates) ->
    encode_plain_text(?CHANGE_CIPHER_SPEC, Version, <<1:8>>, ConnectionStates).

encode_plain_text(Type, Version, Data, ConnectionStates, RenegotiateAt) ->
    #connection_states{current_write = 
		       #connection_state{sequence_number = Num}} = ConnectionStates,
    case renegotiate(Num, RenegotiateAt) of
	false ->
	    encode_plain_text(Type, Version, Data, ConnectionStates);
	true ->
	    {renegotiate, Data}
    end.

encode_plain_text(Type, Version, Data, ConnectionStates) ->
    #connection_states{current_write=#connection_state{
			 compression_state=CompS0,
			 security_parameters=
			 #security_parameters{compression_algorithm=CompAlg}
			}=CS0} = ConnectionStates,
    {Comp, CompS1} = compress(CompAlg, Data, CompS0),
    CS1 = CS0#connection_state{compression_state = CompS1},
    {CipherText, CS2} = cipher(Type, Version, Comp, CS1),
    CTBin = encode_tls_cipher_text(Type, Version, CipherText),
    {CTBin, ConnectionStates#connection_states{current_write = CS2}}.

renegotiate(N, M) when N < M->
    false;
renegotiate(_,_) ->
    true.

encode_tls_cipher_text(Type, {MajVer, MinVer}, Fragment) ->
    Length = erlang:iolist_size(Fragment),
    [<<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Length)>>, Fragment].

cipher(Type, Version, Fragment, CS0) ->
    Length = erlang:iolist_size(Fragment),
    {MacHash, CS1=#connection_state{cipher_state = CipherS0,
				 security_parameters=
				 #security_parameters{bulk_cipher_algorithm = 
						      BCA}
				}} = 
	hash_and_bump_seqno(CS0, Type, Version, Length, Fragment),
    ?DBG_HEX(Fragment),
    {Ciphered, CipherS1} = ssl_cipher:cipher(BCA, CipherS0, MacHash, Fragment),
    ?DBG_HEX(Ciphered),
    CS2 = CS1#connection_state{cipher_state=CipherS1},
    {Ciphered, CS2}.

decipher(TLS=#ssl_tls{type=Type, version=Version, fragment=Fragment}, CS0) ->
    SP = CS0#connection_state.security_parameters,
    BCA = SP#security_parameters.bulk_cipher_algorithm, 
    HashSz = SP#security_parameters.hash_size,
    CipherS0 = CS0#connection_state.cipher_state,
    case ssl_cipher:decipher(BCA, HashSz, CipherS0, Fragment, Version) of
	{T, Mac, CipherS1} ->
	    CS1 = CS0#connection_state{cipher_state = CipherS1},
	    TLength = size(T),
	    {MacHash, CS2} = hash_and_bump_seqno(CS1, Type, Version, TLength, T),
	    case is_correct_mac(Mac, MacHash) of
		true ->		  
		    {TLS#ssl_tls{fragment = T}, CS2};
		false ->
		    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
	    end;
	#alert{} = Alert ->
	    Alert
    end.

uncompress(?NULL, Data = #ssl_tls{type = _Type,
				  version = _Version,
				  fragment = _Fragment}, CS) ->
    {Data, CS}.

compress(?NULL, Data, CS) ->
    {Data, CS}.

hash_and_bump_seqno(#connection_state{sequence_number = SeqNo,
				      mac_secret = MacSecret,
				      security_parameters = 
				      SecPars} = CS0,
		    Type, Version, Length, Fragment) ->
    Hash = mac_hash(Version, 
		    SecPars#security_parameters.mac_algorithm,
		    MacSecret, SeqNo, Type,
		    Length, Fragment),
    {Hash, CS0#connection_state{sequence_number = SeqNo+1}}.

is_correct_mac(Mac, Mac) ->
    true;
is_correct_mac(_M,_H) ->
    false.

mac_hash({_,_}, ?NULL, _MacSecret, _SeqNo, _Type,
	 _Length, _Fragment) ->
    <<>>;
mac_hash({3, 0}, MacAlg, MacSecret, SeqNo, Type, Length, Fragment) ->
    ssl_ssl3:mac_hash(MacAlg, MacSecret, SeqNo, Type, Length, Fragment);
mac_hash({3, N} = Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment)  
  when N =:= 1; N =:= 2 ->
    ssl_tls1:mac_hash(MacAlg, MacSecret, SeqNo, Type, Version, 
		      Length, Fragment).
