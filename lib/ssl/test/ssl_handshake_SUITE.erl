%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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

-module(ssl_handshake_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ssl_internal.hrl").
-include("tls_handshake.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> [decode_hello_handshake,
	  decode_single_hello_extension_correctly,
	  decode_unknown_hello_extension_correctly,
	  encode_single_hello_sni_extension_correctly].

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
decode_hello_handshake(_Config) ->
    HelloPacket = <<16#02, 16#00, 16#00,
		    16#44, 16#03, 16#03, 16#4e, 16#7f, 16#c1, 16#03, 16#35,
		    16#c2, 16#07, 16#b9, 16#4a, 16#58, 16#af, 16#34, 16#07,
		    16#a6, 16#7e, 16#ef, 16#52, 16#cb, 16#e0, 16#ea, 16#b7,
		    16#aa, 16#47, 16#c8, 16#c2, 16#2c, 16#66, 16#fa, 16#f8,
		    16#09, 16#42, 16#cf, 16#00, 16#c0, 16#30, 16#00, 16#00,
		    16#1c,
		    16#00, 16#0b, 16#00, 16#04, 16#03, 16#00, 16#01, 16#02, % ec_point_formats
		    16#ff, 16#01, 16#00, 16#01, 16#00, %% renegotiate
		    16#00, 16#23,
		    16#00, 16#00, 16#33, 16#74, 16#00, 16#07, 16#06, 16#73,
		    16#70, 16#64, 16#79, 16#2f, 16#32>>,
	
    Version = {3, 0},
    {Records, _Buffer} = tls_handshake:get_tls_handshake(Version, HelloPacket, <<>>),

    {Hello, _Data} = hd(Records),
    #renegotiation_info{renegotiated_connection = <<0>>}
	= (Hello#server_hello.extensions)#hello_extensions.renegotiation_info.

decode_single_hello_extension_correctly(_Config) -> 
    Renegotiation = <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(1), 0>>,
    Extensions = ssl_handshake:decode_hello_extensions(Renegotiation),
    #renegotiation_info{renegotiated_connection = <<0>>}
	= Extensions#hello_extensions.renegotiation_info.


decode_unknown_hello_extension_correctly(_Config) ->
    FourByteUnknown = <<16#CA,16#FE, ?UINT16(4), 3, 0, 1, 2>>,
    Renegotiation = <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(1), 0>>,
    Extensions = ssl_handshake:decode_hello_extensions(<<FourByteUnknown/binary, Renegotiation/binary>>),
     #renegotiation_info{renegotiated_connection = <<0>>}
	= Extensions#hello_extensions.renegotiation_info.

encode_single_hello_sni_extension_correctly(_Config) ->
    Exts = #hello_extensions{sni = #sni{hostname = "test.com"}},
    SNI = <<16#00, 16#00, 16#00, 16#0d, 16#00, 16#0b, 16#00, 16#00, 16#08,
	    $t,    $e,    $s,    $t,    $.,    $c,    $o,    $m>>,
    ExtSize = byte_size(SNI),
    HelloExt = <<ExtSize:16/unsigned-big-integer, SNI/binary>>,
    Encoded = ssl_handshake:encode_hello_extensions(Exts),
    HelloExt = Encoded.
