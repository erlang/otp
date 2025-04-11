%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
-moduledoc false.

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include("ssh_auth.hrl").
-include("ssh_transport.hrl").

-export([encode/1, decode/1, decode_keyboard_interactive_prompts/2]).
-export([ssh2_pubkey_decode/1,
         ssh2_pubkey_encode/1,
         ssh2_privkey_decode2/1,
         oid2ssh_curvename/1,
         ssh_curvename2oid/1,
         %% experimental:
         ssh2_privkey_encode/1
        ]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2]).
-define(ALG_NAME_LIMIT, 64).

ucl(B) ->
    try unicode:characters_to_list(B) of
	L when is_list(L) -> L;
	{error,_Matched,_Rest} -> throw({error,bad_unicode})
    catch
	_:_ -> throw({error,bad_unicode})
    end.

-define(unicode_list(B), ucl(B)).

%%%================================================================
%%%
%%% Encode/decode messages
%%% 

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
					  language = Lang
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
    <<?Ebyte(?SSH_MSG_SERVICE_REQUEST), ?Estring(Service)>>;

encode(#ssh_msg_service_accept{
	  name = Service
	 }) ->
    <<?Ebyte(?SSH_MSG_SERVICE_ACCEPT), ?Estring(Service)>>;

encode(#ssh_msg_ext_info{
          nr_extensions = N,
          data = Data
         }) ->
    lists:foldl(fun({ExtName,ExtVal}, Acc) ->
                        <<Acc/binary, ?Estring(ExtName), ?Estring(ExtVal)>>
                end,
                <<?Ebyte(?SSH_MSG_EXT_INFO), ?Euint32(N)>>,
                Data);

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
	  public_host_key = {Key,SigAlg},
	  f = F,
	  h_sig = Signature
	 }) ->
    EncKey = ssh2_pubkey_encode(Key),
    EncSign = encode_signature(Key, SigAlg, Signature),
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
	  public_host_key = {Key,SigAlg},
	  f = F,
	  h_sig = Signature
	 }) ->
    EncKey = ssh2_pubkey_encode(Key),
    EncSign = encode_signature(Key, SigAlg, Signature),
    <<?Ebyte(?SSH_MSG_KEX_DH_GEX_REPLY), ?Ebinary(EncKey), ?Empint(F), ?Ebinary(EncSign)>>;

encode(#ssh_msg_kex_ecdh_init{q_c = Q_c}) ->
    <<?Ebyte(?SSH_MSG_KEX_ECDH_INIT), ?Ebinary(Q_c)>>;

encode(#ssh_msg_kex_ecdh_reply{public_host_key = {Key,SigAlg}, q_s = Q_s, h_sig = Sign}) ->
    EncKey = ssh2_pubkey_encode(Key),
    EncSign = encode_signature(Key, SigAlg, Sign),
    <<?Ebyte(?SSH_MSG_KEX_ECDH_REPLY), ?Ebinary(EncKey), ?Ebinary(Q_s), ?Ebinary(EncSign)>>;

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
	 ?DEC_BIN(RequestType,__0), ?BYTE(Bool), Data/binary>>=Bytes) ->
    try
        #ssh_msg_channel_request{
           recipient_channel = Recipient,
           request_type = binary:bin_to_list(RequestType),
           want_reply = erl_boolean(Bool),
           data  = Data
          }
    catch _:_ ->
            %% Faulty, RFC4254 says:
            %% "If the request is not recognized or is not
            %% supported for the channel, SSH_MSG_CHANNEL_FAILURE is returned."
            %% So we provoke such a message to be sent
            #ssh_msg_channel_request{
               recipient_channel = Recipient,
               request_type = faulty_msg,
               data = Bytes
              }
    end;
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
       service = binary:bin_to_list(Service),
       method =  binary:bin_to_list(Method),
       data = Data
      };

decode(<<?BYTE(?SSH_MSG_USERAUTH_FAILURE),
	 ?DEC_BIN(Auths,__0),
	 ?BYTE(Bool)>>) ->
    #ssh_msg_userauth_failure {
       authentications = binary:bin_to_list(Auths),
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
       language = Lang
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

decode(<<?BYTE(?SSH_MSG_EXT_INFO), ?UINT32(N), BinData/binary>>) ->
    Data = bin_foldr(
             fun(Bin,Acc) when length(Acc) == N ->
                     {Bin,Acc};
                (<<?DEC_BIN(V0,__0), ?DEC_BIN(V1,__1), Rest/binary>>, Acc) -> 
                     {Rest,[{binary_to_list(V0),binary_to_list(V1)}|Acc]}
             end, [], BinData),
    #ssh_msg_ext_info{
       nr_extensions = N,
       data = Data
      };

%%% Keyexchange messages
decode(<<?BYTE(?SSH_MSG_KEXINIT), Cookie:128, Data/binary>>) ->
    decode_kex_init(Data, [Cookie, ssh_msg_kexinit], 10);

decode(<<"dh",?BYTE(?SSH_MSG_KEXDH_INIT), ?DEC_MPINT(E,__0)>>) ->
    #ssh_msg_kexdh_init{e = E
		       };

decode(<<"dh", ?BYTE(?SSH_MSG_KEXDH_REPLY), ?DEC_BIN(Key,__0), ?DEC_MPINT(F,__1), ?DEC_BIN(Hashsign,__2)>>) ->
    #ssh_msg_kexdh_reply{
       public_host_key = ssh2_pubkey_decode(Key),
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
       public_host_key = ssh2_pubkey_decode(Key),
       f = F,
       h_sig = decode_signature(Hashsign)
      };

decode(<<"ecdh",?BYTE(?SSH_MSG_KEX_ECDH_INIT), ?DEC_BIN(Q_c,__0)>>) ->
    #ssh_msg_kex_ecdh_init{
       q_c = Q_c
      };

decode(<<"ecdh",?BYTE(?SSH_MSG_KEX_ECDH_REPLY),
	 ?DEC_BIN(Key,__1), ?DEC_BIN(Q_s,__2), ?DEC_BIN(Sig,__3)>>) ->
    #ssh_msg_kex_ecdh_reply{
       public_host_key = ssh2_pubkey_decode(Key),
       q_s = Q_s,
       h_sig = decode_signature(Sig)
      };

decode(<<?SSH_MSG_SERVICE_REQUEST, ?DEC_BIN(Service,__0)>>) ->
    #ssh_msg_service_request{
       name = binary:bin_to_list(Service)
      };

decode(<<?SSH_MSG_SERVICE_ACCEPT, ?DEC_BIN(Service,__0)>>) ->
    #ssh_msg_service_accept{
       name = binary:bin_to_list(Service)
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

%% Accept SSH_MSG_IGNORE without data to have feature parity with other implementations like openssh
decode(<<?BYTE(?SSH_MSG_IGNORE)>>) ->
    #ssh_msg_ignore{};
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
%%% Encode/decode ssh public/private keys
%%%

%%%-------- public key --------
ssh2_pubkey_encode(#'RSAPublicKey'{modulus = N, publicExponent = E}) ->
    <<?STRING(<<"ssh-rsa">>), ?Empint(E), ?Empint(N)>>;

ssh2_pubkey_encode({Y,  #'Dss-Parms'{p = P, q = Q, g = G}}) ->
    <<?STRING(<<"ssh-dss">>), ?Empint(P), ?Empint(Q), ?Empint(G), ?Empint(Y)>>;

ssh2_pubkey_encode({#'ECPoint'{point = Q}, {namedCurve,OID}}) when OID == ?'id-Ed25519' orelse
                                                                   OID == ?'id-Ed448' ->
    {KeyType, _} = oid2ssh_curvename(OID),
    <<?STRING(KeyType), ?Estring(Q)>>;

ssh2_pubkey_encode(#'ECPrivateKey'{parameters = {namedCurve,OID},
                                   publicKey = Key}) when OID == ?'id-Ed25519' orelse
                                                          OID == ?'id-Ed448' ->
    {KeyType, _} = oid2ssh_curvename(OID),
    <<?STRING(KeyType), ?Estring(Key)>>;

ssh2_pubkey_encode(#'ECPrivateKey'{parameters = {namedCurve,OID},
                                   publicKey = Key}) ->
    {KeyType,Curve} = oid2ssh_curvename(OID),
    <<?STRING(KeyType), ?STRING(Curve), ?Estring(Key)>>;

ssh2_pubkey_encode({#'ECPoint'{point = Q}, {namedCurve,OID}}) ->
    {KeyType,Curve} = oid2ssh_curvename(OID),
    <<?STRING(KeyType), ?STRING(Curve), ?Estring(Q)>>.

%%%--------
ssh2_pubkey_decode(KeyBlob) ->
    {Key,_RestBlob} = ssh2_pubkey_decode2(KeyBlob),
    Key.
    
ssh2_pubkey_decode2(<<?UINT32(7), "ssh-rsa",
                      ?DEC_INT(E, _EL),
                      ?DEC_INT(N, _NL),
                      Rest/binary>>) ->
    {#'RSAPublicKey'{modulus = N,
                     publicExponent = E
                    }, Rest};
ssh2_pubkey_decode2(<<?UINT32(7), "ssh-dss",
                      ?DEC_INT(P, _PL),
                      ?DEC_INT(Q, _QL),
                      ?DEC_INT(G, _GL),
                      ?DEC_INT(Y, _YL),
                      Rest/binary>>) ->
    {{Y, #'Dss-Parms'{p = P,
                      q = Q,
                      g = G}
     }, Rest};

ssh2_pubkey_decode2(<<?DEC_BIN(SshCurveName,SCNL), Rest0/binary>>) ->
    {Pub, Rest} =
        case {SshCurveName, Rest0} of
            {<<"ecdsa-sha2-", _/binary>>,
             <<?DEC_BIN(_Curve, _IL),
               ?DEC_BIN(Q, _QL),
               Rest1/binary>>} ->  {Q, Rest1};
            
            {<<"ssh-ed",_/binary>>,
             <<?DEC_BIN(Key, _L),
               Rest1/binary>>} ->  {Key, Rest1}
        end,
    OID = ssh_curvename2oid(SshCurveName),
    {{#'ECPoint'{point = Pub}, {namedCurve,OID}},
     Rest}.

%%%-------- private key --------

%% dialyser... ssh2_privkey_decode(KeyBlob) ->
%% dialyser...     {Key,_RestBlob} = ssh2_privkey_decode2(KeyBlob),
%% dialyser...     Key.
%% See sshkey_private_serialize_opt in sshkey.c

ssh2_privkey_encode(#'RSAPrivateKey'
                    {version = 'two-prime', % Found this in public_key:generate_key/1 ..
                     modulus = N,
                     publicExponent = E,
                     privateExponent = D,
                     prime1 = P,
                     prime2 = Q,
                     %% exponent1, % D_mod_P_1
                     %% exponent2, % D_mod_Q_1
                     coefficient = IQMP
                    }) ->
    <<?STRING(<<"ssh-rsa">>),
      ?Empint(N), % Yes, N and E is reversed relative pubkey format
      ?Empint(E), % --"--
      ?Empint(D),
      ?Empint(IQMP),
      ?Empint(P),
      ?Empint(Q)>>;

ssh2_privkey_encode(#'DSAPrivateKey'
                    {version = 0,
                     p = P,
                     q = Q,
                     g = G,
                     y = Y,
                     x = X
                    }) ->
    <<?STRING(<<"ssh-dss">>),
      ?Empint(P),
      ?Empint(Q),
      ?Empint(G),
      ?Empint(Y), % Publ key
      ?Empint(X)  % Priv key
    >>;

ssh2_privkey_encode(#'ECPrivateKey'
                    {version = 1,
                     parameters = {namedCurve,OID},
                     privateKey = Priv,
                     publicKey = Pub
                    }) when OID == ?'id-Ed25519' orelse
                            OID == ?'id-Ed448' ->
    {CurveName,_} = oid2ssh_curvename(OID),
    <<?STRING(CurveName),
      ?STRING(Pub),
      ?STRING(Priv)>>;

ssh2_privkey_encode(#'ECPrivateKey'
                    {version = 1,
                     parameters = {namedCurve,OID},
                     privateKey = Priv,
                     publicKey = Q
                    }) ->
    {CurveName,_} = oid2ssh_curvename(OID),
    <<?STRING(CurveName),
      ?STRING(CurveName), % SIC!
      ?STRING(Q),
      ?STRING(Priv)>>.
      
%%%--------
ssh2_privkey_decode2(<<?UINT32(7), "ssh-rsa",
                       ?DEC_INT(N, _NL), % Yes, N and E is reversed relative pubkey format
                       ?DEC_INT(E, _EL), % --"--
                       ?DEC_INT(D, _DL),
                       ?DEC_INT(IQMP, _IQMPL),
                       ?DEC_INT(P, _PL),
                       ?DEC_INT(Q, _QL),
                       Rest/binary>>) ->
    {#'RSAPrivateKey'{version = 'two-prime', % Found this in public_key:generate_key/1 ..
                      modulus = N,
                      publicExponent = E,
                      privateExponent = D,
                      prime1 = P,
                      prime2 = Q,
                      %exponent1, % D_mod_P_1
                      %exponent2, % D_mod_Q_1
                      coefficient = IQMP
                     }, Rest};
ssh2_privkey_decode2(<<?UINT32(7), "ssh-dss",
                       ?DEC_INT(P, _PL),
                       ?DEC_INT(Q, _QL),
                       ?DEC_INT(G, _GL),
                       ?DEC_INT(Y, _YL), % Publ key
                       ?DEC_INT(X, _XL), % Priv key
                       Rest/binary>>) ->
    {#'DSAPrivateKey'{version = 0,
                      p = P,
                      q = Q,
                      g = G,
                      y = Y,
                      x = X
                     }, Rest};

ssh2_privkey_decode2(<<?DEC_BIN(SshCurveName,SCNL), Rest0/binary>>) ->
    {Pub, Priv, Rest} =
        case {SshCurveName, Rest0} of
            {<<"ecdsa-sha2-",_/binary>>,
             <<?DEC_BIN(_Curve, _IL),
               ?DEC_BIN(Pub1, _QL),
               ?DEC_BIN(Priv1, _PrivL),
               Rest1/binary>>} ->
                {Pub1, Priv1, Rest1};

            {<<"ssh-ed",_/binary>>,
             <<?DEC_BIN(Pub1, PL),
               ?DEC_BIN(PrivPub, PPL),
               Rest1/binary>>} ->
               PL = PPL div 2,
                <<Priv1:PL/binary, _/binary>> = PrivPub,
                {Pub1, Priv1, Rest1}
        end,
    OID = ssh_curvename2oid(SshCurveName),
    {#'ECPrivateKey'{version = 1,
                     parameters = {namedCurve,OID},
                     privateKey = Priv,
                     publicKey = Pub
                    }, Rest}.


%% Description: Converts from the ssh name of elliptic curves to
%% the OIDs.
%%--------------------------------------------------------------------
ssh_curvename2oid(<<"ssh-ed25519">>) -> ?'id-Ed25519';
ssh_curvename2oid(<<"ssh-ed448">>  ) -> ?'id-Ed448';
ssh_curvename2oid(<<"ecdsa-sha2-nistp256">>) -> ?'secp256r1';
ssh_curvename2oid(<<"ecdsa-sha2-nistp384">>) -> ?'secp384r1';
ssh_curvename2oid(<<"ecdsa-sha2-nistp521">>) -> ?'secp521r1'.

%% Description: Converts from elliptic curve OIDs to the ssh name.
%%--------------------------------------------------------------------
oid2ssh_curvename(?'id-Ed25519')-> {<<"ssh-ed25519">>, 'n/a'};
oid2ssh_curvename(?'id-Ed448')  -> {<<"ssh-ed448">>,   'n/a'};
oid2ssh_curvename(?'secp256r1') -> {<<"ecdsa-sha2-nistp256">>, <<"nistp256">>};
oid2ssh_curvename(?'secp384r1') -> {<<"ecdsa-sha2-nistp384">>, <<"nistp384">>};
oid2ssh_curvename(?'secp521r1') -> {<<"ecdsa-sha2-nistp521">>, <<"nistp521">>}.

%%%================================================================
%%%
%%% Helper functions
%%%

bin_foldr(Fun, Acc, Bin) ->
    lists:reverse(bin_foldl(Fun, Acc, Bin)).

bin_foldl(_, Acc, <<>>) -> Acc;
bin_foldl(Fun, Acc0, Bin0) ->
    case Fun(Bin0,Acc0) of
        {Bin0,Acc0} ->
            Acc0;
        {Bin,Acc} ->
            bin_foldl(Fun, Acc, Bin)
    end.

%%%----------------------------------------------------------------
decode_keyboard_interactive_prompts(<<>>, Acc) ->
    lists:reverse(Acc);
decode_keyboard_interactive_prompts(<<0>>, Acc) ->
    lists:reverse(Acc);
decode_keyboard_interactive_prompts(<<?DEC_BIN(Prompt,__0), ?BYTE(Bool), Bin/binary>>,
				    Acc) ->
    decode_keyboard_interactive_prompts(Bin, [{Prompt, erl_boolean(Bool)} | Acc]).

%%%----------------------------------------------------------------
erl_boolean(0) ->
    false;
erl_boolean(1) ->
    true.

%%%----------------------------------------------------------------
decode_kex_init(<<?BYTE(Bool), ?UINT32(X)>>, Acc, 0) ->
    list_to_tuple(lists:reverse([X, erl_boolean(Bool) | Acc]));
decode_kex_init(<<?BYTE(Bool)>>, Acc, 0) ->
    %% The mandatory trailing UINT32 is missing. Assume the value it anyhow must have
    %% See rfc 4253 7.1
    X = 0,
    list_to_tuple(lists:reverse([X, erl_boolean(Bool) | Acc]));
decode_kex_init(<<?DEC_BIN(Data,__0), Rest/binary>>, Acc, N) ->
    BinParts = binary:split(Data, <<$,>>, [global]),
    Process =
        fun(<<>>, PAcc) ->
                PAcc;
           (Part, PAcc) ->
                case byte_size(Part) > ?ALG_NAME_LIMIT of
                    true ->
                        ?LOG_DEBUG("Ignoring too long name", []),
                        PAcc;
                    false ->
                        Name = binary:bin_to_list(Part),
                        [Name | PAcc]
                end
        end,
    Names = lists:foldr(Process, [], BinParts),
    decode_kex_init(Rest, [Names | Acc], N - 1).


%%%================================================================
%%%
%%% Signature decode/encode
%%%

decode_signature(<<?DEC_BIN(Alg,__0), ?UINT32(_), Signature/binary>>) ->
    {binary_to_list(Alg), Signature}.


encode_signature(#'RSAPublicKey'{}, SigAlg, Signature) ->
    SignName = list_to_binary(atom_to_list(SigAlg)),
    <<?Ebinary(SignName), ?Ebinary(Signature)>>;
encode_signature({_, #'Dss-Parms'{}}, _SigAlg, Signature) ->
    <<?Ebinary(<<"ssh-dss">>), ?Ebinary(Signature)>>;
encode_signature({#'ECPoint'{}, {namedCurve,OID}}, _SigAlg, Signature) ->
    {SshCurveName,_} = oid2ssh_curvename(OID),
    <<?Ebinary(<<SshCurveName/binary>>), ?Ebinary(Signature)>>.


%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [ssh_messages, raw_messages].

ssh_dbg_flags(ssh_messages) -> [c];
ssh_dbg_flags(raw_messages) -> [c].

ssh_dbg_on(P) when P==ssh_messages ;
                   P==raw_messages ->
    dbg:tp(?MODULE,encode,1,x),
    dbg:tp(?MODULE,decode,1,x).

ssh_dbg_off(P) when P==ssh_messages ;
                    P==raw_messages ->
    dbg:ctpg(?MODULE,encode,1),
    dbg:ctpg(?MODULE,decode,1).

ssh_dbg_format(ssh_messages, {call,{?MODULE,encode,[Msg]}}) ->
    Name = string:to_upper(atom_to_list(element(1,Msg))),
    ["Going to send ",Name,":\n",
     wr_record(ssh_dbg:shrink_bin(Msg))
    ];
ssh_dbg_format(ssh_messages, {return_from, {?MODULE,encode,1}, _Ret}) ->
    skip;

ssh_dbg_format(ssh_messages, {call, {?MODULE,decode,[_]}}) ->
    skip;
ssh_dbg_format(ssh_messages, {return_from,{?MODULE,decode,1},Msg}) ->
    Name = string:to_upper(atom_to_list(element(1,Msg))),
    ["Received ",Name,":\n",
     wr_record(ssh_dbg:shrink_bin(Msg)),
     case Msg of
         #ssh_msg_userauth_request{service = "ssh-connection",
                                   method = "publickey",
                                   data = <<_,?DEC_BIN(Alg,__0),_/binary>>} ->
             io_lib:format("  data decoded: ~s ... ~n", [Alg]);

         #ssh_msg_channel_request{request_type = "env",
                                  data = <<?DEC_BIN(Var,__0),?DEC_BIN(Val,__1)>>} ->
             io_lib:format("  data decoded: ~s = ~s~n", [Var, Val]);

         #ssh_msg_channel_request{request_type = "exec",
                                  data = <<?DEC_BIN(Cmnd,__0)>>} ->
             io_lib:format("  data decoded: ~s~n", [Cmnd]);

         #ssh_msg_channel_request{request_type = "pty-req",
                                  data = <<?DEC_BIN(BTermName,_TermLen),
                                           ?UINT32(Width),?UINT32(Height),
                                           ?UINT32(PixWidth), ?UINT32(PixHeight),
                                           Modes/binary>>} ->
             io_lib:format("  data decoded: terminal = ~s~n"
                           "                width x height = ~p x ~p~n"
                           "                pix-width x pix-height = ~p x ~p~n"
                           "                pty-opts = ~p~n",
                           [BTermName, Width,Height, PixWidth, PixHeight,
                            ssh_connection:decode_pty_opts(Modes)]);
         _ ->
             ""
     end
    ];

ssh_dbg_format(raw_messages, {call,{?MODULE,decode,[BytesPT]}}) ->
    ["Received plain text bytes (shown after decryption):\n",
     io_lib:format("~p",[BytesPT])
    ];
ssh_dbg_format(raw_messages, {return_from, {?MODULE,decode,1}, _Ret}) ->
    skip;

ssh_dbg_format(raw_messages, {call, {?MODULE,encode,[_]}}) ->
    skip;
ssh_dbg_format(raw_messages, {return_from,{?MODULE,encode,1},BytesPT}) ->
    ["Going to send plain text bytes (shown before encryption):\n",
     io_lib:format("~p",[BytesPT])
    ].


?wr_record(ssh_msg_disconnect);
?wr_record(ssh_msg_ignore);
?wr_record(ssh_msg_unimplemented);
?wr_record(ssh_msg_debug);
?wr_record(ssh_msg_service_request);
?wr_record(ssh_msg_service_accept);
?wr_record(ssh_msg_kexinit);
?wr_record(ssh_msg_kexdh_init);
?wr_record(ssh_msg_kexdh_reply);
?wr_record(ssh_msg_newkeys);
?wr_record(ssh_msg_ext_info);
?wr_record(ssh_msg_kex_dh_gex_request);
?wr_record(ssh_msg_kex_dh_gex_request_old);
?wr_record(ssh_msg_kex_dh_gex_group);
?wr_record(ssh_msg_kex_dh_gex_init);
?wr_record(ssh_msg_kex_dh_gex_reply);
?wr_record(ssh_msg_kex_ecdh_init);
?wr_record(ssh_msg_kex_ecdh_reply);

?wr_record(ssh_msg_userauth_request);
?wr_record(ssh_msg_userauth_failure);
?wr_record(ssh_msg_userauth_success);
?wr_record(ssh_msg_userauth_banner);
?wr_record(ssh_msg_userauth_passwd_changereq);
?wr_record(ssh_msg_userauth_pk_ok);
?wr_record(ssh_msg_userauth_info_request);
?wr_record(ssh_msg_userauth_info_response);

?wr_record(ssh_msg_global_request);
?wr_record(ssh_msg_request_success);
?wr_record(ssh_msg_request_failure);
?wr_record(ssh_msg_channel_open);
?wr_record(ssh_msg_channel_open_confirmation);
?wr_record(ssh_msg_channel_open_failure);
?wr_record(ssh_msg_channel_window_adjust);
?wr_record(ssh_msg_channel_data);
?wr_record(ssh_msg_channel_extended_data);
?wr_record(ssh_msg_channel_eof);
?wr_record(ssh_msg_channel_close);
?wr_record(ssh_msg_channel_request);
?wr_record(ssh_msg_channel_success);
?wr_record(ssh_msg_channel_failure);

wr_record(R) -> io_lib:format('~p~n',[R]).
