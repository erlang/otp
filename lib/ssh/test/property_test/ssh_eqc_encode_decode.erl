%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

-module(ssh_eqc_encode_decode).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
%%-define(PROPER,true).
%%-define(TRIQ,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.

%% Public key records:
-include_lib("public_key/include/public_key.hrl").

%%% Properties:

prop_ssh_decode() ->
    ?FORALL({Msg,KexFam}, ?LET(KF, kex_family(), {ssh_msg(KF),KF} ),
	    try ssh_message:decode(decode_state(Msg,KexFam))
	    of
		_ -> true
	    catch

		C:E -> io:format('~p:~p~n',[C,E]),
		       false
	    end
	   ).


%%% This fails because ssh_message is not symmetric in encode and decode regarding data types
prop_ssh_decode_encode() ->
    ?FORALL({Msg,KexFam}, ?LET(KF, kex_family(), {ssh_msg(KF),KF} ),
	    Msg == ssh_message:encode(
		     fix_asym(
		       ssh_message:decode(decode_state(Msg,KexFam))))
	   ).


%%%================================================================
%%%
%%% Generators
%%% 

ssh_msg(<<"dh">>) ->
    ?LET(M,oneof(
	     [
	      [msg_code('SSH_MSG_KEXDH_INIT'),gen_mpint()],  % 30
	      [msg_code('SSH_MSG_KEXDH_REPLY'),gen_pubkey_string(rsa),gen_mpint(),gen_signature_string(rsa)] % 31
	      | rest_ssh_msgs()
	     ]),
	 list_to_binary(M));

ssh_msg(<<"dh_gex">>) ->
    ?LET(M,oneof(
	     [
	      [msg_code('SSH_MSG_KEX_DH_GEX_REQUEST_OLD'),gen_uint32()],  % 30
	      [msg_code('SSH_MSG_KEX_DH_GEX_GROUP'),gen_mpint(),gen_mpint()]  % 31
	      | rest_ssh_msgs()
	     ]),
	 list_to_binary(M));

 ssh_msg(<<"ecdh">>) ->
     ?LET(M,oneof(
 	     [
 	      [msg_code('SSH_MSG_KEX_ECDH_INIT'),gen_mpint()],   % 30
	      [msg_code('SSH_MSG_KEX_ECDH_REPLY'),gen_pubkey_string(ecdsa),gen_mpint(),gen_signature_string(ecdsa)] % 31
	      | rest_ssh_msgs()
 	     ]),
	 list_to_binary(M)).


rest_ssh_msgs() -> 
    [%%                    SSH_MSG_USERAUTH_INFO_RESPONSE
     %% hard args          SSH_MSG_USERAUTH_INFO_REQUEST
     %% rfc4252 p12 error  SSH_MSG_USERAUTH_REQUEST
     [msg_code('SSH_MSG_KEX_DH_GEX_REQUEST'),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_KEX_DH_GEX_INIT'),gen_mpint()],
     [msg_code('SSH_MSG_KEX_DH_GEX_REPLY'),gen_pubkey_string(rsa),gen_mpint(),gen_signature_string(rsa)],
     [msg_code('SSH_MSG_CHANNEL_CLOSE'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_DATA'),gen_uint32(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_EOF'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_EXTENDED_DATA'),gen_uint32(),gen_uint32(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_FAILURE'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("direct-tcpip"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("forwarded-tcpip"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("session"),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string("x11"),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN'),gen_string( ),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN_CONFIRMATION'),gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_OPEN_FAILURE'),gen_uint32(),gen_uint32(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("env"),gen_boolean(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exec"),gen_boolean(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exit-signal"),0,gen_string( ),gen_boolean(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("exit-status"),0,gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("pty-req"),gen_boolean(),gen_string( ),gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("shell"),gen_boolean()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("signal"),0,gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("subsystem"),gen_boolean(),gen_string( )],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("window-change"),0,gen_uint32(),gen_uint32(),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("x11-req"),gen_boolean(),gen_boolean(),gen_string( ),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string("xon-xoff"),0,gen_boolean()],
     [msg_code('SSH_MSG_CHANNEL_REQUEST'),gen_uint32(),gen_string( ),gen_boolean()],
     [msg_code('SSH_MSG_CHANNEL_SUCCESS'),gen_uint32()],
     [msg_code('SSH_MSG_CHANNEL_WINDOW_ADJUST'),gen_uint32(),gen_uint32()],
     [msg_code('SSH_MSG_DEBUG'),gen_boolean(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_DISCONNECT'),gen_uint32(),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string("cancel-tcpip-forward"),gen_boolean(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string("tcpip-forward"),gen_boolean(),gen_string( ),gen_uint32()],
     [msg_code('SSH_MSG_GLOBAL_REQUEST'),gen_string( ),gen_boolean()],
     [msg_code('SSH_MSG_IGNORE'),gen_string( )],
     [msg_code('SSH_MSG_KEXINIT'),gen_byte(16),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_name_list(),gen_boolean(),gen_uint32()],
     [msg_code('SSH_MSG_NEWKEYS')],
     [msg_code('SSH_MSG_REQUEST_FAILURE')],
     [msg_code('SSH_MSG_REQUEST_SUCCESS')],
     [msg_code('SSH_MSG_REQUEST_SUCCESS'),gen_uint32()],
     [msg_code('SSH_MSG_SERVICE_ACCEPT'),gen_string( )],
     [msg_code('SSH_MSG_SERVICE_REQUEST'),gen_string( )],
     [msg_code('SSH_MSG_UNIMPLEMENTED'),gen_uint32()],
     [msg_code('SSH_MSG_USERAUTH_BANNER'),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_USERAUTH_FAILURE'),gen_name_list(),gen_boolean()],
     [msg_code('SSH_MSG_USERAUTH_PASSWD_CHANGEREQ'),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_USERAUTH_PK_OK'),gen_string( ),gen_string( )],
     [msg_code('SSH_MSG_USERAUTH_SUCCESS')]
    ].

kex_family() -> oneof([<<"dh">>, <<"dh_gex">>, <<"ecdh">>]).

gen_boolean() -> choose(0,1).

gen_byte() -> choose(0,255).

gen_uint16() -> gen_byte(2).

gen_uint32() -> gen_byte(4).

gen_uint64() -> gen_byte(8).

gen_byte(N) when N>0 -> [gen_byte() || _ <- lists:seq(1,N)].
    
gen_char() -> choose($a,$z).

gen_mpint() -> ?LET(I, largeint(), ssh_bits:mpint(I)).

strip_0s([0|T]) -> strip_0s(T);
strip_0s(X) -> X.
    

gen_string() -> 
    ?LET(Size, choose(0,10), 
	 ?LET(Vector,vector(Size, gen_char()),
	      gen_string(Vector) 
	     )).

gen_string(S) when is_binary(S) -> gen_string(binary_to_list(S));
gen_string(S) when is_list(S) -> uint32_to_list(length(S)) ++ S.
    
gen_name_list() ->
    ?LET(NumNames, choose(0,10),
	 ?LET(L, [gen_name() || _ <- lists:seq(1,NumNames)],
	      gen_string( string:join(L,"," ) )
	)).

gen_name() -> gen_string().

uint32_to_list(I) ->  binary_to_list(<<I:32/unsigned-big-integer>>).
    
gen_pubkey_string(Type) ->
    PubKey = case Type of
		 rsa -> #'RSAPublicKey'{modulus = 12345,publicExponent = 2};
		 ecdsa -> {#'ECPoint'{point=[1,2,3,4,5]},
			   {namedCurve,{1,2,840,10045,3,1,7}}} % 'secp256r1' nistp256
	     end,
    gen_string(public_key:ssh_encode(PubKey, ssh2_pubkey)).

    
gen_signature_string(Type) ->
    Signature = <<"hejhopp">>,
    Id = case Type of
	     rsa -> "ssh-rsa";
	     ecdsa -> "ecdsa-sha2-nistp256"
	 end,
    gen_string(gen_string(Id) ++ gen_string(Signature)).

-define(MSG_CODE(Name,Num),
msg_code(Name) -> Num;
msg_code(Num) -> Name
).

?MSG_CODE('SSH_MSG_USERAUTH_REQUEST',   50);
?MSG_CODE('SSH_MSG_USERAUTH_FAILURE',   51);
?MSG_CODE('SSH_MSG_USERAUTH_SUCCESS',   52);
?MSG_CODE('SSH_MSG_USERAUTH_BANNER',   53);
?MSG_CODE('SSH_MSG_USERAUTH_PK_OK',   60);
?MSG_CODE('SSH_MSG_USERAUTH_PASSWD_CHANGEREQ',   60);
?MSG_CODE('SSH_MSG_DISCONNECT',   1);
?MSG_CODE('SSH_MSG_IGNORE',   2);
?MSG_CODE('SSH_MSG_UNIMPLEMENTED',   3);
?MSG_CODE('SSH_MSG_DEBUG',   4);
?MSG_CODE('SSH_MSG_SERVICE_REQUEST',   5);
?MSG_CODE('SSH_MSG_SERVICE_ACCEPT',   6);
?MSG_CODE('SSH_MSG_KEXINIT',   20);
?MSG_CODE('SSH_MSG_NEWKEYS',   21);
?MSG_CODE('SSH_MSG_GLOBAL_REQUEST',   80);
?MSG_CODE('SSH_MSG_REQUEST_SUCCESS',   81);
?MSG_CODE('SSH_MSG_REQUEST_FAILURE',   82);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN',   90);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN_CONFIRMATION',   91);
?MSG_CODE('SSH_MSG_CHANNEL_OPEN_FAILURE',   92);
?MSG_CODE('SSH_MSG_CHANNEL_WINDOW_ADJUST',   93);
?MSG_CODE('SSH_MSG_CHANNEL_DATA',   94);
?MSG_CODE('SSH_MSG_CHANNEL_EXTENDED_DATA',   95);
?MSG_CODE('SSH_MSG_CHANNEL_EOF',   96);
?MSG_CODE('SSH_MSG_CHANNEL_CLOSE',   97);
?MSG_CODE('SSH_MSG_CHANNEL_REQUEST',   98);
?MSG_CODE('SSH_MSG_CHANNEL_SUCCESS',   99);
?MSG_CODE('SSH_MSG_CHANNEL_FAILURE',   100);
?MSG_CODE('SSH_MSG_USERAUTH_INFO_REQUEST',   60);
?MSG_CODE('SSH_MSG_USERAUTH_INFO_RESPONSE',   61);
?MSG_CODE('SSH_MSG_KEXDH_INIT', 30);
?MSG_CODE('SSH_MSG_KEXDH_REPLY', 31);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REQUEST_OLD',   30);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REQUEST',   34);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_GROUP',   31);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_INIT',   32);
?MSG_CODE('SSH_MSG_KEX_DH_GEX_REPLY', 33);
?MSG_CODE('SSH_MSG_KEX_ECDH_INIT', 30);
?MSG_CODE('SSH_MSG_KEX_ECDH_REPLY', 31).

%%%====================================================
%%%=== WARNING: Knowledge of the test object ahead! ===
%%%====================================================

%% SSH message records:
-include_lib("ssh/src/ssh_connect.hrl").
-include_lib("ssh/src/ssh_transport.hrl").

%%% Encoding and decodeing is asymetric so out=binary in=string. Sometimes. :(
-define(fix_asym_Xdh_reply(S),
 fix_asym(#S{public_host_key = Key, h_sig = {Alg,Sig}} = M) ->
      M#S{public_host_key = {Key, list_to_atom(Alg)}, h_sig = Sig}
).


fix_asym(#ssh_msg_global_request{name=N} = M) -> M#ssh_msg_global_request{name = binary_to_list(N)};
fix_asym(#ssh_msg_debug{message=D,language=L} = M) -> M#ssh_msg_debug{message = binary_to_list(D),
								      language = binary_to_list(L)};
fix_asym(#ssh_msg_kexinit{cookie=C} = M) -> M#ssh_msg_kexinit{cookie = <<C:128>>};
?fix_asym_Xdh_reply(ssh_msg_kexdh_reply);
?fix_asym_Xdh_reply(ssh_msg_kex_dh_gex_reply);
?fix_asym_Xdh_reply(ssh_msg_kex_ecdh_reply);
fix_asym(M) -> M.


%%% Message codes 30 and 31 are overloaded depending on kex family so arrange the decoder
%%% input as the test object does
decode_state(<<30,_/binary>>=Msg, KexFam) -> <<KexFam/binary, Msg/binary>>;
decode_state(<<31,_/binary>>=Msg, KexFam) -> <<KexFam/binary, Msg/binary>>;
decode_state(Msg, _) -> Msg.

