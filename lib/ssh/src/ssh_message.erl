%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

%%------------------------------------------------------------------
-module(ssh_message).

-include_lib("public_key/include/public_key.hrl").

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include("ssh_auth.hrl").
-include("ssh_transport.hrl").

-export([encode/1, decode/1, decode_keyboard_interactive_prompts/2]).

-define('2bin'(X), (if is_binary(X) -> X;
		       is_list(X) -> list_to_binary(X);
		       X==undefined -> <<>>
		    end) ).

-define('E...'(X),    ?'2bin'(X)/binary ).
-define(Eboolean(X),  ?BOOLEAN(case X of
				   true -> ?TRUE;
				   false -> ?FALSE
			       end) ).
-define(Ebyte(X),        ?BYTE(X) ).
-define(Euint32(X),      ?UINT32(X) ).
-define(Estring(X),      ?STRING(?'2bin'(X)) ).
-define(Estring_utf8(X), ?string_utf8(X)/binary ).
-define(Ename_list(X),   ?STRING(ssh_bits:name_list(X)) ).
-define(Empint(X),       (ssh_bits:mpint(X))/binary ).
-define(Ebinary(X),      ?STRING(X) ).

ucl(B) ->
    try unicode:characters_to_list(B) of
	L when is_list(L) -> L;
	{error,_Matched,Rest} -> throw({error,{bad_unicode,Rest}})
    catch
	_:_ -> throw({error,bad_unicode})
    end.

-define(unicode_list(B), ucl(B)).

encode(#ssh_msg_global_request{
	  name = Name,
	  want_reply = Bool,
	  data = Data}) ->
    <<?Ebyte(?SSH_MSG_GLOBAL_REQUEST), ?Estring(Name), ?Eboolean(Bool), ?'E...'(Data)>>;

encode(#ssh_msg_request_success{data = Data}) ->
    <<?Ebyte(?SSH_MSG_REQUEST_SUCCESS), Data/binary>>;

encode(#ssh_msg_request_failure{}) ->
    <<?Ebyte(?SSH_MSG_REQUEST_FAILURE)>>;

encode(#ssh_msg_channel_open{
	  channel_type = Type,
	  sender_channel = Sender,
	  initial_window_size = Window,
	  maximum_packet_size = Max,
	  data = Data
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_OPEN), ?Estring(Type), ?Euint32(Sender), ?Euint32(Window), ?Euint32(Max), ?'E...'(Data)>>;

encode(#ssh_msg_channel_open_confirmation{
	  recipient_channel = Recipient,
	  sender_channel = Sender,
	  initial_window_size = InitWindowSize,
	  maximum_packet_size = MaxPacketSize,
	  data = Data
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_OPEN_CONFIRMATION),
      ?Euint32(Recipient), ?Euint32(Sender), ?Euint32(InitWindowSize), ?Euint32(MaxPacketSize),
      ?'E...'(Data)>>;

encode(#ssh_msg_channel_open_failure{
	  recipient_channel = Recipient,
	  reason = Reason,
	  description = Desc,
	  lang = Lang
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_OPEN_FAILURE), ?Euint32(Recipient),?Euint32(Reason), ?Estring(Desc), ?Estring(Lang)>>;

encode(#ssh_msg_channel_window_adjust{
	  recipient_channel = Recipient,
	  bytes_to_add = Bytes
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_WINDOW_ADJUST), ?Euint32(Recipient), ?Euint32(Bytes)>>;

encode(#ssh_msg_channel_data{
	  recipient_channel = Recipient,
	  data = Data
	 }) ->
   <<?Ebyte(?SSH_MSG_CHANNEL_DATA), ?Euint32(Recipient), ?Ebinary(Data)>>;

encode(#ssh_msg_channel_extended_data{
	  recipient_channel = Recipient,
	  data_type_code = DataType,
	  data = Data
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_EXTENDED_DATA), ?Euint32(Recipient), ?Euint32(DataType), ?Ebinary(Data)>>;

encode(#ssh_msg_channel_eof{recipient_channel = Recipient
			   }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_EOF), ?Euint32(Recipient)>>;

encode(#ssh_msg_channel_close{
	   recipient_channel = Recipient
	  }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_CLOSE), ?Euint32(Recipient)>>;

encode(#ssh_msg_channel_request{
	  recipient_channel = Recipient,
	  request_type = Type,
	  want_reply = Bool,
	  data  = Data
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_REQUEST), ?Euint32(Recipient), ?Estring(Type), ?Eboolean(Bool), ?'E...'(Data)>>;

encode(#ssh_msg_channel_success{
	  recipient_channel = Recipient
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_SUCCESS), ?Euint32(Recipient)>>;

encode(#ssh_msg_channel_failure{
	  recipient_channel = Recipient
	 }) ->
    <<?Ebyte(?SSH_MSG_CHANNEL_FAILURE), ?Euint32(Recipient)>>;

encode(#ssh_msg_userauth_request{
	  user = User,
	  service = Service,
	  method = Method,
	  data = Data
	 }) ->
    <<?Ebyte(?SSH_MSG_USERAUTH_REQUEST), ?Estring_utf8(User), ?Estring(Service), ?Estring(Method), ?'E...'(Data)>>;

encode(#ssh_msg_userauth_failure{
	  authentications = Auths,
	  partial_success = Bool
	 }) ->
    <<?Ebyte(?SSH_MSG_USERAUTH_FAILURE), ?Estring(Auths), ?Eboolean(Bool)>>;

encode(#ssh_msg_userauth_success{}) ->
    <<?Ebyte(?SSH_MSG_USERAUTH_SUCCESS)>>;

encode(#ssh_msg_userauth_banner{
       message = Banner,
       language = Lang
      }) ->
    <<?Ebyte(?SSH_MSG_USERAUTH_BANNER), ?Estring_utf8(Banner), ?Estring(Lang)>>;

encode(#ssh_msg_userauth_pk_ok{
	  algorithm_name = Alg,
	  key_blob = KeyBlob
	 }) ->
    <<?Ebyte(?SSH_MSG_USERAUTH_PK_OK), ?Estring(Alg), ?Ebinary(KeyBlob)>>;

encode(#ssh_msg_userauth_passwd_changereq{prompt = Prompt,
					  languge = Lang
					 })->
    <<?Ebyte(?SSH_MSG_USERAUTH_PASSWD_CHANGEREQ), ?Estring_utf8(Prompt), ?Estring(Lang)>>;

encode(#ssh_msg_userauth_info_request{
	  name = Name,
	  instruction = Inst,
	  language_tag = Lang,
	  num_prompts = NumPromtps,
	  data = Data}) ->
    <<?Ebyte(?SSH_MSG_USERAUTH_INFO_REQUEST), ?Estring_utf8(Name), ?Estring_utf8(Inst), ?Estring(Lang),
      ?Euint32(NumPromtps), ?'E...'(Data)>>;

encode(#ssh_msg_userauth_info_response{
	  num_responses = Num,
	  data = Data}) ->
    lists:foldl(fun %%("", Acc) -> Acc;  % commented out since it seem wrong
		    (Response, Acc) -> <<Acc/binary, ?Estring_utf8(Response)>>
		end,
		<<?Ebyte(?SSH_MSG_USERAUTH_INFO_RESPONSE), ?Euint32(Num)>>,
		Data);

encode(#ssh_msg_disconnect{
	  code = Code,
	  description = Desc,
	  language = Lang
	 }) ->
    <<?Ebyte(?SSH_MSG_DISCONNECT), ?Euint32(Code), ?Estring_utf8(Desc), ?Estring(Lang)>>;

encode(#ssh_msg_service_request{
	  name = Service
	 }) ->
    <<?Ebyte(?SSH_MSG_SERVICE_REQUEST), ?Estring_utf8(Service)>>;

encode(#ssh_msg_service_accept{
	  name = Service
	 }) ->
    <<?Ebyte(?SSH_MSG_SERVICE_ACCEPT), ?Estring_utf8(Service)>>;

encode(#ssh_msg_newkeys{}) ->
    <<?Ebyte(?SSH_MSG_NEWKEYS)>>;

encode(#ssh_msg_kexinit{
	  cookie = Cookie,
	  kex_algorithms = KeyAlgs,
	  server_host_key_algorithms = HostKeyAlgs,
	  encryption_algorithms_client_to_server = EncAlgC2S,
	  encryption_algorithms_server_to_client = EncAlgS2C,
	  mac_algorithms_client_to_server = MacAlgC2S,
	  mac_algorithms_server_to_client = MacAlgS2C,
	  compression_algorithms_client_to_server = CompAlgS2C,
	  compression_algorithms_server_to_client = CompAlgC2S,
	  languages_client_to_server = LangC2S,
	  languages_server_to_client = LangS2C,
	  first_kex_packet_follows = Bool,
	  reserved = Reserved
	 }) ->
    <<?Ebyte(?SSH_MSG_KEXINIT), Cookie/binary,
      ?Ename_list(KeyAlgs), ?Ename_list(HostKeyAlgs), ?Ename_list(EncAlgC2S), ?Ename_list(EncAlgS2C), ?Ename_list(MacAlgC2S),
      ?Ename_list(MacAlgS2C), ?Ename_list(CompAlgS2C), ?Ename_list(CompAlgC2S), ?Ename_list(LangC2S), ?Ename_list(LangS2C),
      ?Eboolean(Bool), ?Euint32(Reserved)>>;

encode(#ssh_msg_kexdh_init{e = E}) ->
    <<?Ebyte(?SSH_MSG_KEXDH_INIT), ?Empint(E)>>;

encode(#ssh_msg_kexdh_reply{
	  public_host_key = Key,
	  f = F,
	  h_sig = Signature
	 }) ->
    EncKey = public_key:ssh_encode(Key, ssh2_pubkey),
    EncSign = encode_signature(Key, Signature),
    <<?Ebyte(?SSH_MSG_KEXDH_REPLY), ?Ebinary(EncKey), ?Empint(F), ?Ebinary(EncSign)>>;

encode(#ssh_msg_kex_dh_gex_request{
	  min = Min,
	  n = N,
	  max = Max
	 }) ->
    <<?Ebyte(?SSH_MSG_KEX_DH_GEX_REQUEST), ?Euint32(Min), ?Euint32(N), ?Euint32(Max)>>;

encode(#ssh_msg_kex_dh_gex_request_old{n = N}) ->
    <<?Ebyte(?SSH_MSG_KEX_DH_GEX_REQUEST_OLD), ?Euint32(N)>>;

encode(#ssh_msg_kex_dh_gex_group{p = Prime, g = Generator}) ->
    <<?Ebyte(?SSH_MSG_KEX_DH_GEX_GROUP), ?Empint(Prime), ?Empint(Generator)>>;

encode(#ssh_msg_kex_dh_gex_init{e = Public}) ->
    <<?Ebyte(?SSH_MSG_KEX_DH_GEX_INIT), ?Empint(Public)>>;

encode(#ssh_msg_kex_dh_gex_reply{
	  %% Will be private key encode_host_key extracts only the public part!
	  public_host_key = Key,
	  f = F,
	  h_sig = Signature
	 }) ->
    EncKey = public_key:ssh_encode(Key, ssh2_pubkey),
    EncSign = encode_signature(Key, Signature),
    <<?Ebyte(?SSH_MSG_KEX_DH_GEX_REPLY), ?Ebinary(EncKey), ?Empint(F), ?Ebinary(EncSign)>>;

encode(#ssh_msg_kex_ecdh_init{q_c = Q_c}) ->
    <<?Ebyte(?SSH_MSG_KEX_ECDH_INIT), ?Empint(Q_c)>>;

encode(#ssh_msg_kex_ecdh_reply{public_host_key = Key, q_s = Q_s, h_sig = Sign}) ->
    EncKey = public_key:ssh_encode(Key, ssh2_pubkey),
    EncSign = encode_signature(Key, Sign),
    <<?Ebyte(?SSH_MSG_KEX_ECDH_REPLY), ?Ebinary(EncKey), ?Empint(Q_s), ?Ebinary(EncSign)>>;

encode(#ssh_msg_ignore{data = Data}) ->
    <<?Ebyte(?SSH_MSG_IGNORE), ?Estring_utf8(Data)>>;

encode(#ssh_msg_unimplemented{sequence = Seq}) ->
    <<?Ebyte(?SSH_MSG_UNIMPLEMENTED), ?Euint32(Seq)>>;

encode(#ssh_msg_debug{always_display = Bool,
		      message = Msg,
		      language = Lang}) ->
    <<?Ebyte(?SSH_MSG_DEBUG), ?Eboolean(Bool), ?Estring_utf8(Msg), ?Estring(Lang)>>.


%% Connection Messages
decode(<<?BYTE(?SSH_MSG_GLOBAL_REQUEST), ?DEC_BIN(Name,__0), ?BYTE(Bool), Data/binary>>) ->
    #ssh_msg_global_request{
       name = Name,
       want_reply = erl_boolean(Bool),
       data = Data
      };
decode(<<?BYTE(?SSH_MSG_REQUEST_SUCCESS), Data/binary>>) ->
    #ssh_msg_request_success{data = Data};
decode(<<?BYTE(?SSH_MSG_REQUEST_FAILURE)>>) ->
    #ssh_msg_request_failure{};
decode(<<?BYTE(?SSH_MSG_CHANNEL_OPEN),
	 ?DEC_BIN(Type,__0), ?UINT32(Sender), ?UINT32(Window), ?UINT32(Max),
	 Data/binary>>) ->
    #ssh_msg_channel_open{
       channel_type = binary_to_list(Type),
       sender_channel = Sender,
       initial_window_size = Window,
       maximum_packet_size = Max,
       data = Data
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_OPEN_CONFIRMATION), ?UINT32(Recipient), ?UINT32(Sender),
	 ?UINT32(InitWindowSize), ?UINT32(MaxPacketSize),
	 Data/binary>>) ->
    #ssh_msg_channel_open_confirmation{
       recipient_channel = Recipient,
       sender_channel = Sender,
       initial_window_size = InitWindowSize,
       maximum_packet_size = MaxPacketSize,
       data = Data
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_OPEN_FAILURE),  ?UINT32(Recipient), ?UINT32(Reason),
	 ?DEC_BIN(Desc,__0), ?DEC_BIN(Lang,__1) >> ) ->
    #ssh_msg_channel_open_failure{
       recipient_channel = Recipient,
       reason = Reason,
       description = ?unicode_list(Desc),
       lang = Lang
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_WINDOW_ADJUST), ?UINT32(Recipient), ?UINT32(Bytes)>>) ->
    #ssh_msg_channel_window_adjust{
       recipient_channel = Recipient,
       bytes_to_add = Bytes
    };

decode(<<?BYTE(?SSH_MSG_CHANNEL_DATA), ?UINT32(Recipient), ?DEC_BIN(Data,__0)>>) ->
    #ssh_msg_channel_data{
       recipient_channel = Recipient,
       data = Data
    };
decode(<<?BYTE(?SSH_MSG_CHANNEL_EXTENDED_DATA), ?UINT32(Recipient), 
	 ?UINT32(DataType), ?DEC_BIN(Data,__0)>>) ->
    #ssh_msg_channel_extended_data{
       recipient_channel = Recipient,
       data_type_code = DataType,
       data = Data
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_EOF), ?UINT32(Recipient)>>) ->
    #ssh_msg_channel_eof{
       recipient_channel = Recipient
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_CLOSE),  ?UINT32(Recipient)>>) ->
    #ssh_msg_channel_close{
       recipient_channel = Recipient
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_REQUEST), ?UINT32(Recipient),
	 ?DEC_BIN(RequestType,__0), ?BYTE(Bool), Data/binary>>) ->
    #ssh_msg_channel_request{
       recipient_channel = Recipient,
       request_type = ?unicode_list(RequestType),
       want_reply = erl_boolean(Bool),
       data  = Data
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_SUCCESS),  ?UINT32(Recipient)>>) ->
    #ssh_msg_channel_success{
       recipient_channel = Recipient
      };
decode(<<?BYTE(?SSH_MSG_CHANNEL_FAILURE),  ?UINT32(Recipient)>>) ->
    #ssh_msg_channel_failure{
       recipient_channel = Recipient
      };

%%% Auth Messages
decode(<<?BYTE(?SSH_MSG_USERAUTH_REQUEST),
	 ?DEC_BIN(User,__0), ?DEC_BIN(Service,__1), ?DEC_BIN(Method,__2),
	 Data/binary>>) ->
    #ssh_msg_userauth_request{
       user =    ?unicode_list(User),
       service = ?unicode_list(Service),
       method =  ?unicode_list(Method),
       data = Data
      };

decode(<<?BYTE(?SSH_MSG_USERAUTH_FAILURE),
	 ?DEC_BIN(Auths,__0),
	 ?BYTE(Bool)>>) ->
    #ssh_msg_userauth_failure {
       authentications = ?unicode_list(Auths),
       partial_success = erl_boolean(Bool)
      };

decode(<<?BYTE(?SSH_MSG_USERAUTH_SUCCESS)>>) ->
    #ssh_msg_userauth_success{};

decode(<<?BYTE(?SSH_MSG_USERAUTH_BANNER), ?DEC_BIN(Banner,__0), ?DEC_BIN(Lang,__1) >>) ->
    #ssh_msg_userauth_banner{
       message = Banner,
       language = Lang
      };

decode(<<?BYTE(?SSH_MSG_USERAUTH_INFO_REQUEST),
	 ?DEC_BIN(Name,__0), ?DEC_BIN(Inst,__1), ?DEC_BIN(Lang,__2),
	 ?UINT32(NumPromtps), Data/binary>>) ->
    #ssh_msg_userauth_info_request{
       name = Name,
       instruction = Inst,
       language_tag = Lang,
       num_prompts = NumPromtps,
       data = Data};

%%% Unhandled message, also masked by same 1:st byte value as ?SSH_MSG_USERAUTH_INFO_REQUEST:
decode(<<?BYTE(?SSH_MSG_USERAUTH_PASSWD_CHANGEREQ), ?DEC_BIN(Prompt,__0), ?DEC_BIN(Lang,__1) >>) ->
    #ssh_msg_userauth_passwd_changereq{
       prompt = Prompt,
       languge = Lang
      };

%%% Unhandled message, also masked by same 1:st byte value as ?SSH_MSG_USERAUTH_INFO_REQUEST:
decode(<<?BYTE(?SSH_MSG_USERAUTH_PK_OK), ?DEC_BIN(Alg,__0), KeyBlob/binary>>) ->
    #ssh_msg_userauth_pk_ok{
       algorithm_name = Alg,
       key_blob = KeyBlob
      };

decode(<<?BYTE(?SSH_MSG_USERAUTH_INFO_RESPONSE), ?UINT32(Num), Data/binary>>) ->
    #ssh_msg_userauth_info_response{
       num_responses = Num,
       data = Data};

%%% Keyexchange messages
decode(<<?BYTE(?SSH_MSG_KEXINIT), Cookie:128, Data/binary>>) ->
    decode_kex_init(Data, [Cookie, ssh_msg_kexinit], 10);

decode(<<"dh",?BYTE(?SSH_MSG_KEXDH_INIT), ?DEC_MPINT(E,__0)>>) ->
    #ssh_msg_kexdh_init{e = E
		       };

decode(<<"dh", ?BYTE(?SSH_MSG_KEXDH_REPLY), ?DEC_BIN(Key,__0), ?DEC_MPINT(F,__1), ?DEC_BIN(Hashsign,__2)>>) ->
    #ssh_msg_kexdh_reply{
       public_host_key = public_key:ssh_decode(Key, ssh2_pubkey),
       f = F,
       h_sig = decode_signature(Hashsign)
      };

decode(<<?BYTE(?SSH_MSG_KEX_DH_GEX_REQUEST), ?UINT32(Min), ?UINT32(N), ?UINT32(Max)>>) ->
    #ssh_msg_kex_dh_gex_request{
       min = Min,
       n = N,
       max = Max
      };

decode(<<"dh_gex",?BYTE(?SSH_MSG_KEX_DH_GEX_REQUEST_OLD), ?UINT32(N)>>) ->
    #ssh_msg_kex_dh_gex_request_old{
       n = N
      };

decode(<<"dh_gex",?BYTE(?SSH_MSG_KEX_DH_GEX_GROUP), ?DEC_MPINT(Prime,__0), ?DEC_MPINT(Generator,__1) >>) ->
    #ssh_msg_kex_dh_gex_group{
       p = Prime,
       g = Generator
      };

decode(<<?BYTE(?SSH_MSG_KEX_DH_GEX_INIT), ?DEC_MPINT(E,__0)>>) ->
    #ssh_msg_kex_dh_gex_init{
       e = E
      };

decode(<<?BYTE(?SSH_MSG_KEX_DH_GEX_REPLY), ?DEC_BIN(Key,__0), ?DEC_MPINT(F,__1), ?DEC_BIN(Hashsign,__2)>>) ->
    #ssh_msg_kex_dh_gex_reply{
       public_host_key = public_key:ssh_decode(Key, ssh2_pubkey),
       f = F,
       h_sig = decode_signature(Hashsign)
      };

decode(<<"ecdh",?BYTE(?SSH_MSG_KEX_ECDH_INIT), ?DEC_MPINT(Q_c,__0)>>) ->
    #ssh_msg_kex_ecdh_init{
       q_c = Q_c
      };

decode(<<"ecdh",?BYTE(?SSH_MSG_KEX_ECDH_REPLY),
	 ?DEC_BIN(Key,__1), ?DEC_MPINT(Q_s,__2), ?DEC_BIN(Sig,__3)>>) ->
    #ssh_msg_kex_ecdh_reply{
       public_host_key = public_key:ssh_decode(Key, ssh2_pubkey),
       q_s = Q_s,
       h_sig = decode_signature(Sig)
      };

decode(<<?SSH_MSG_SERVICE_REQUEST, ?DEC_BIN(Service,__0)>>) ->
    #ssh_msg_service_request{
       name = ?unicode_list(Service)
      };

decode(<<?SSH_MSG_SERVICE_ACCEPT, ?DEC_BIN(Service,__0)>>) ->
    #ssh_msg_service_accept{
       name = ?unicode_list(Service)
      };

decode(<<?BYTE(?SSH_MSG_DISCONNECT), ?UINT32(Code), ?DEC_BIN(Desc,__0), ?DEC_BIN(Lang,__1)>>) ->
    #ssh_msg_disconnect{
       code = Code,
       description = ?unicode_list(Desc),
       language = Lang
      };

%% Accept bad disconnects from ancient openssh clients that doesn't send language tag.  Use english as a work-around.
decode(<<?BYTE(?SSH_MSG_DISCONNECT), ?UINT32(Code), ?DEC_BIN(Desc,__0)>>) ->
    #ssh_msg_disconnect{
       code = Code,
       description = ?unicode_list(Desc),
       language = <<"en">>
      };

decode(<<?SSH_MSG_NEWKEYS>>) ->
    #ssh_msg_newkeys{};

decode(<<?BYTE(?SSH_MSG_IGNORE), ?DEC_BIN(Data,__0)>>) ->
    #ssh_msg_ignore{data = Data};

decode(<<?BYTE(?SSH_MSG_UNIMPLEMENTED), ?UINT32(Seq)>>) ->
    #ssh_msg_unimplemented{sequence = Seq};

decode(<<?BYTE(?SSH_MSG_DEBUG), ?BYTE(Bool), ?DEC_BIN(Msg,__0), ?DEC_BIN(Lang,__1)>>) ->
    #ssh_msg_debug{always_display = erl_boolean(Bool),
		   message = Msg,
		   language = Lang}.

%%%================================================================
%%%
%%% Helper functions
%%%

decode_keyboard_interactive_prompts(<<>>, Acc) ->
    lists:reverse(Acc);
decode_keyboard_interactive_prompts(<<?DEC_BIN(Prompt,__0), ?BYTE(Bool), Bin/binary>>,
				    Acc) ->
    decode_keyboard_interactive_prompts(Bin, [{Prompt, erl_boolean(Bool)} | Acc]).

erl_boolean(0) ->
    false;
erl_boolean(1) ->
    true.

decode_kex_init(<<?BYTE(Bool), ?UINT32(X)>>, Acc, 0) ->
    list_to_tuple(lists:reverse([X, erl_boolean(Bool) | Acc]));
decode_kex_init(<<?BYTE(Bool)>>, Acc, 0) ->
    %% The mandatory trailing UINT32 is missing. Assume the value it anyhow must have
    %% See rfc 4253 7.1
    X = 0,
    list_to_tuple(lists:reverse([X, erl_boolean(Bool) | Acc]));
decode_kex_init(<<?DEC_BIN(Data,__0), Rest/binary>>, Acc, N) ->
    Names = string:tokens(?unicode_list(Data), ","),
    decode_kex_init(Rest, [Names | Acc], N -1).


%%%================================================================
%%%
%%% Signature decode/encode
%%%

decode_signature(<<?DEC_BIN(_Alg,__0), ?UINT32(_), Signature/binary>>) ->
    Signature.


encode_signature(#'RSAPublicKey'{}, Signature) ->
    <<?Ebinary(<<"ssh-rsa">>), ?Ebinary(Signature)>>;
encode_signature({_, #'Dss-Parms'{}}, Signature) ->
    <<?Ebinary(<<"ssh-dss">>), ?Ebinary(Signature)>>;
encode_signature({#'ECPoint'{}, {namedCurve,OID}}, Signature) ->
    CurveName = public_key:oid2ssh_curvename(OID),
    <<?Ebinary(<<"ecdsa-sha2-",CurveName/binary>>), ?Ebinary(Signature)>>.

