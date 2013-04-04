%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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
%% Purpose: Record and constant defenitions for the SSL ciphers and
%% the SSL-cipher protocol see RFC 4346, RFC 3268
%%----------------------------------------------------------------------

-ifndef(ssl_cipher).
-define(ssl_cipher, true).

-type cipher()            :: null |rc4_128 | idea_cbc | des40_cbc | des_cbc | '3des_ede_cbc' 
			   | aes_128_cbc |  aes_256_cbc.
-type hash()              :: null | sha | md5 | sha256 | sha384 | sha512.
-type erl_cipher_suite()  :: {key_algo(), cipher(), hash()}.
-type int_cipher_suite()  :: {key_algo(), cipher(), hash(), hash()}.
-type cipher_suite()      :: binary().
-type cipher_enum()        :: integer().
-type openssl_cipher_suite()  :: string().

%%% SSL cipher protocol  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(CHANGE_CIPHER_SPEC_PROTO, 1).           % _PROTO to not clash with 
						% SSL record protocol

-record(change_cipher_spec, {
	  type = 1
	 }).

%%% SSL cipher suites %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -record(cipher_state, 
%% 	{
%% 	  suite,
%% 	  name,
%% 	  state
%% 	 }).

-record(cipher_state, {
	  iv,
	  key,
	  state
	 }).

%%% TLS_NULL_WITH_NULL_NULL is specified and is the initial state of a
%%% TLS connection during the first handshake on that channel, but
%%% must not be negotiated, as it provides no more protection than an
%%% unsecured connection.  

%% TLS_NULL_WITH_NULL_NULL = { 0x00,0x00 };
-define(TLS_NULL_WITH_NULL_NULL, <<?BYTE(16#00), ?BYTE(16#00)>>).

%%% The following cipher suite definitions require that the server
%%% provide an RSA certificate that can be used for key exchange. The
%%% server may request either an RSA or a DSS signature-capable
%%% certificate in the certificate request message.

%%      TLS_RSA_WITH_NULL_MD5 = { 0x00,0x01 };
-define(TLS_RSA_WITH_NULL_MD5, <<?BYTE(16#00), ?BYTE(16#01)>>).

%%      TLS_RSA_WITH_NULL_SHA = { 0x00,0x02 };
-define(TLS_RSA_WITH_NULL_SHA, <<?BYTE(16#00), ?BYTE(16#02)>>).

%%      TLS_RSA_WITH_RC4_128_MD5 = { 0x00,0x04 };
-define(TLS_RSA_WITH_RC4_128_MD5, <<?BYTE(16#00), ?BYTE(16#04)>>).

%%      TLS_RSA_WITH_RC4_128_SHA = { 0x00,0x05 };
-define(TLS_RSA_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#05)>>).

%%      TLS_RSA_WITH_IDEA_CBC_SHA = { 0x00,0x07 };
-define(TLS_RSA_WITH_IDEA_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#07)>>).

%%      TLS_RSA_WITH_DES_CBC_SHA = { 0x00,0x09 };
-define(TLS_RSA_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#09)>>).

%%      TLS_RSA_WITH_3DES_EDE_CBC_SHA = { 0x00,0x0A };
-define(TLS_RSA_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#0A)>>).

%%% The following CipherSuite definitions are used for server-
%%% authenticated (and optionally client-authenticated)
%%% Diffie-Hellman. DH denotes cipher suites in which the server's
%%% certificate contains the Diffie-Hellman parameters signed by the
%%% certificate authority (CA). DHE denotes ephemeral Diffie-Hellman,
%%% where the Diffie-Hellman parameters are signed by a DSS or RSA
%%% certificate, which has been signed by the CA. The signing
%%% algorithm used is specified after the DH or DHE parameter. The
%%% server can request an RSA or DSS signature- capable certificate
%%% from the client for client authentication or it may request a
%%% Diffie-Hellman certificate. Any Diffie-Hellman certificate
%%% provided by the client must use the parameters (group and
%%% generator) described by the server.

%%	TLS_DH_DSS_WITH_DES_CBC_SHA            = { 0x00,0x0C };
-define(TLS_DH_DSS_WITH_DES_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#0C)>>).

%%	TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA       = { 0x00,0x0D };
-define(TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#0D)>>).

%%	TLS_DH_RSA_WITH_DES_CBC_SHA            = { 0x00,0x0F };
-define(TLS_DH_RSA_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#0F)>>).

%%	TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA       = { 0x00,0x10 };
-define(TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#10)>>).

%%	TLS_DHE_DSS_WITH_DES_CBC_SHA           = { 0x00,0x12 };
-define(TLS_DHE_DSS_WITH_DES_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#12)>>).

%%	TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA      = { 0x00,0x13 };
-define(TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#13)>>).

%%	TLS_DHE_RSA_WITH_DES_CBC_SHA           = { 0x00,0x15 };
-define(TLS_DHE_RSA_WITH_DES_CBC_SHA,  <<?BYTE(16#00), ?BYTE(16#15)>>).

%%	TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA      = { 0x00,0x16 };
-define(TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA,   <<?BYTE(16#00), ?BYTE(16#16)>>).

%%	TLS_DH_anon_WITH_RC4_128_MD5           = { 0x00,0x18 };
-define(TLS_DH_anon_WITH_RC4_128_MD5,   <<?BYTE(16#00),?BYTE(16#18)>>).

%%	TLS_DH_anon_WITH_DES_CBC_SHA           = { 0x00,0x1A };
-define(TLS_DH_anon_WITH_DES_CBC_SHA,   <<?BYTE(16#00), ?BYTE(16#1A)>>).

%%	TLS_DH_anon_WITH_3DES_EDE_CBC_SHA      = { 0x00,0x1B };
-define(TLS_DH_anon_WITH_3DES_EDE_CBC_SHA,   <<?BYTE(16#00), ?BYTE(16#1B)>>).


%%% AES Cipher Suites RFC 3268

%%	TLS_RSA_WITH_AES_128_CBC_SHA      = { 0x00, 0x2F };
-define(TLS_RSA_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#2F)>>).   

%%	TLS_DH_DSS_WITH_AES_128_CBC_SHA   = { 0x00, 0x30 };
-define(TLS_DH_DSS_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#30)>>).   
   
%%	TLS_DH_RSA_WITH_AES_128_CBC_SHA   = { 0x00, 0x31 };
-define(TLS_DH_RSA_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#31)>>).      

%%	TLS_DHE_DSS_WITH_AES_128_CBC_SHA  = { 0x00, 0x32 };
-define(TLS_DHE_DSS_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#32)>>).     

%%	TLS_DHE_RSA_WITH_AES_128_CBC_SHA  = { 0x00, 0x33 };
-define(TLS_DHE_RSA_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#33)>>).   
   
%%	TLS_DH_anon_WITH_AES_128_CBC_SHA  = { 0x00, 0x34 };
-define(TLS_DH_anon_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#34)>>).   
   
%%	TLS_RSA_WITH_AES_256_CBC_SHA      = { 0x00, 0x35 };
-define(TLS_RSA_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#35)>>).   
   
%%	TLS_DH_DSS_WITH_AES_256_CBC_SHA   = { 0x00, 0x36 };
-define(TLS_DH_DSS_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#36)>>).   
   
%%	TLS_DH_RSA_WITH_AES_256_CBC_SHA   = { 0x00, 0x37 };
-define(TLS_DH_RSA_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#37)>>).   
   
%%	TLS_DHE_DSS_WITH_AES_256_CBC_SHA  = { 0x00, 0x38 };
-define(TLS_DHE_DSS_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#38)>>).   
   
%%	TLS_DHE_RSA_WITH_AES_256_CBC_SHA  = { 0x00, 0x39 };
-define(TLS_DHE_RSA_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#39)>>).   
   
%%	TLS_DH_anon_WITH_AES_256_CBC_SHA  = { 0x00, 0x3A };
-define(TLS_DH_anon_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#3A)>>).   

%%% TLS 1.2 Cipher Suites RFC 5246

%%      TLS_RSA_WITH_NULL_SHA256              = { 0x00,0x3B };
-define(TLS_RSA_WITH_NULL_SHA256, <<?BYTE(16#00), ?BYTE(16#3B)>>).

%%      TLS_RSA_WITH_AES_128_CBC_SHA256       = { 0x00,0x3C };
-define(TLS_RSA_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#3C)>>).

%%      TLS_RSA_WITH_AES_256_CBC_SHA256       = { 0x00,0x3D };
-define(TLS_RSA_WITH_AES_256_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#3D)>>).

%%      TLS_DH_DSS_WITH_AES_128_CBC_SHA256    = { 0x00,0x3E };
-define(TLS_DH_DSS_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#3E)>>).

%%      TLS_DH_RSA_WITH_AES_128_CBC_SHA256    = { 0x00,0x3F };
-define(TLS_DH_RSA_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#3F)>>).

%%      TLS_DHE_DSS_WITH_AES_128_CBC_SHA256   = { 0x00,0x40 };
-define(TLS_DHE_DSS_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#40)>>).

%%      TLS_DHE_RSA_WITH_AES_128_CBC_SHA256   = { 0x00,0x67 };
-define(TLS_DHE_RSA_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#67)>>).

%%      TLS_DH_DSS_WITH_AES_256_CBC_SHA256    = { 0x00,0x68 };
-define(TLS_DH_DSS_WITH_AES_256_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#68)>>).

%%      TLS_DH_RSA_WITH_AES_256_CBC_SHA256    = { 0x00,0x69 };
-define(TLS_DH_RSA_WITH_AES_256_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#69)>>).

%%      TLS_DHE_DSS_WITH_AES_256_CBC_SHA256   = { 0x00,0x6A };
-define(TLS_DHE_DSS_WITH_AES_256_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#6A)>>).

%%      TLS_DHE_RSA_WITH_AES_256_CBC_SHA256   = { 0x00,0x6B };
-define(TLS_DHE_RSA_WITH_AES_256_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#6B)>>).

%%      TLS_DH_anon_WITH_AES_128_CBC_SHA256   = { 0x00,0x6C };
-define(TLS_DH_anon_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#6C)>>).

%%      TLS_DH_anon_WITH_AES_256_CBC_SHA256   = { 0x00,0x6D };
-define(TLS_DH_anon_WITH_AES_256_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#6D)>>).

%%% Kerberos Cipher Suites 

%%      TLS_KRB5_WITH_DES_CBC_SHA            = { 0x00,0x1E };
-define(TLS_KRB5_WITH_DES_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#1E)>>).
 
%%      TLS_KRB5_WITH_3DES_EDE_CBC_SHA       = { 0x00,0x1F };
-define(TLS_KRB5_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#1F)>>).

%%      TLS_KRB5_WITH_RC4_128_SHA            = { 0x00,0x20 };
-define(TLS_KRB5_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#20)>>). 

%%      TLS_KRB5_WITH_IDEA_CBC_SHA           = { 0x00,0x21 };
-define(TLS_KRB5_WITH_IDEA_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#21)>>).
 
%%      TLS_KRB5_WITH_DES_CBC_MD5            = { 0x00,0x22 };
-define(TLS_KRB5_WITH_DES_CBC_MD5, <<?BYTE(16#00), ?BYTE(16#22)>>). 

%%      TLS_KRB5_WITH_3DES_EDE_CBC_MD5       = { 0x00,0x23 };
-define(TLS_KRB5_WITH_3DES_EDE_CBC_MD5, <<?BYTE(16#00), ?BYTE(16#23)>>). 

%%      TLS_KRB5_WITH_RC4_128_MD5            = { 0x00,0x24 };
-define(TLS_KRB5_WITH_RC4_128_MD5, <<?BYTE(16#00), ?BYTE(16#24)>>). 

%%      TLS_KRB5_WITH_IDEA_CBC_MD5           = { 0x00,0x25 };
-define(TLS_KRB5_WITH_IDEA_CBC_MD5, <<?BYTE(16#00), ?BYTE(16#25)>>).

%% RFC 5746 - Not a real cipher suite used to signal empty "renegotiation_info" extension
%% to avoid handshake failure from old servers that do not ignore
%% hello extension data as they should.
-define(TLS_EMPTY_RENEGOTIATION_INFO_SCSV, <<?BYTE(16#00), ?BYTE(16#FF)>>).

%%% PSK Cipher Suites RFC 4279

%%      TLS_PSK_WITH_RC4_128_SHA              = { 0x00, 0x8A };
-define(TLS_PSK_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#8A)>>).

%%      TLS_PSK_WITH_3DES_EDE_CBC_SHA         = { 0x00, 0x8B };
-define(TLS_PSK_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#8B)>>).

%%      TLS_PSK_WITH_AES_128_CBC_SHA          = { 0x00, 0x8C };
-define(TLS_PSK_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#8C)>>).

%%      TLS_PSK_WITH_AES_256_CBC_SHA          = { 0x00, 0x8D };
-define(TLS_PSK_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#8D)>>).

%%      TLS_DHE_PSK_WITH_RC4_128_SHA          = { 0x00, 0x8E };
-define(TLS_DHE_PSK_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#8E)>>).

%%      TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA     = { 0x00, 0x8F };
-define(TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#8F)>>).

%%      TLS_DHE_PSK_WITH_AES_128_CBC_SHA      = { 0x00, 0x90 };
-define(TLS_DHE_PSK_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#90)>>).

%%      TLS_DHE_PSK_WITH_AES_256_CBC_SHA      = { 0x00, 0x91 };
-define(TLS_DHE_PSK_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#91)>>).

%%      TLS_RSA_PSK_WITH_RC4_128_SHA          = { 0x00, 0x92 };
-define(TLS_RSA_PSK_WITH_RC4_128_SHA, <<?BYTE(16#00), ?BYTE(16#92)>>).

%%      TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA     = { 0x00, 0x93 };
-define(TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#93)>>).

%%      TLS_RSA_PSK_WITH_AES_128_CBC_SHA      = { 0x00, 0x94 };
-define(TLS_RSA_PSK_WITH_AES_128_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#94)>>).

%%      TLS_RSA_PSK_WITH_AES_256_CBC_SHA      = { 0x00, 0x95 };
-define(TLS_RSA_PSK_WITH_AES_256_CBC_SHA, <<?BYTE(16#00), ?BYTE(16#95)>>).

%%% TLS 1.2 PSK Cipher Suites RFC 5487

%%      TLS_PSK_WITH_AES_128_CBC_SHA256       = {0x00,0xAE};
-define(TLS_PSK_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#AE)>>).

%%      TLS_PSK_WITH_AES_256_CBC_SHA384       = {0x00,0xAF};
-define(TLS_PSK_WITH_AES_256_CBC_SHA384, <<?BYTE(16#00), ?BYTE(16#AF)>>).

%%      TLS_PSK_WITH_NULL_SHA256              = {0x00,0xB0};
-define(TLS_PSK_WITH_NULL_SHA256, <<?BYTE(16#00), ?BYTE(16#B0)>>).

%%      TLS_PSK_WITH_NULL_SHA384              = {0x00,0xB1};
-define(TLS_PSK_WITH_NULL_SHA384, <<?BYTE(16#00), ?BYTE(16#B1)>>).

%%      TLS_DHE_PSK_WITH_AES_128_CBC_SHA256   = {0x00,0xB2};
-define(TLS_DHE_PSK_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#B2)>>).

%%      TLS_DHE_PSK_WITH_AES_256_CBC_SHA384   = {0x00,0xB3};
-define(TLS_DHE_PSK_WITH_AES_256_CBC_SHA384, <<?BYTE(16#00), ?BYTE(16#B3)>>).

%%      TLS_DHE_PSK_WITH_NULL_SHA256          = {0x00,0xB4};
-define(TLS_DHE_PSK_WITH_NULL_SHA256, <<?BYTE(16#00), ?BYTE(16#B4)>>).

%%      TLS_DHE_PSK_WITH_NULL_SHA384          = {0x00,0xB5};
-define(TLS_DHE_PSK_WITH_NULL_SHA384, <<?BYTE(16#00), ?BYTE(16#B5)>>).

%%      TLS_RSA_PSK_WITH_AES_128_CBC_SHA256   = {0x00,0xB6};
-define(TLS_RSA_PSK_WITH_AES_128_CBC_SHA256, <<?BYTE(16#00), ?BYTE(16#B6)>>).

%%      TLS_RSA_PSK_WITH_AES_256_CBC_SHA384   = {0x00,0xB7};
-define(TLS_RSA_PSK_WITH_AES_256_CBC_SHA384, <<?BYTE(16#00), ?BYTE(16#B7)>>).

%%      TLS_RSA_PSK_WITH_NULL_SHA256          = {0x00,0xB8};
-define(TLS_RSA_PSK_WITH_NULL_SHA256, <<?BYTE(16#00), ?BYTE(16#B8)>>).

%%      TLS_RSA_PSK_WITH_NULL_SHA384          = {0x00,0xB9};
-define(TLS_RSA_PSK_WITH_NULL_SHA384, <<?BYTE(16#00), ?BYTE(16#B9)>>).

%%% SRP Cipher Suites RFC 5054

%%      TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA     = { 0xC0,0x1A };
-define(TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#1A)>>).

%%      TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA = { 0xC0,0x1B };
-define(TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#1B)>>).

%%      TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA = { 0xC0,0x1C };
-define(TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#1C)>>).

%%      TLS_SRP_SHA_WITH_AES_128_CBC_SHA      = { 0xC0,0x1D };
-define(TLS_SRP_SHA_WITH_AES_128_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#1D)>>).

%%      TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA  = { 0xC0,0x1E };
-define(TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#1E)>>).

%%      TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA  = { 0xC0,0x1F };
-define(TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#1F)>>).

%%      TLS_SRP_SHA_WITH_AES_256_CBC_SHA      = { 0xC0,0x20 };
-define(TLS_SRP_SHA_WITH_AES_256_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#20)>>).

%%      TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA  = { 0xC0,0x21 };
-define(TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#21)>>).

%%      TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA  = { 0xC0,0x22 };
-define(TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA, <<?BYTE(16#C0), ?BYTE(16#22)>>).

-endif. % -ifdef(ssl_cipher).
