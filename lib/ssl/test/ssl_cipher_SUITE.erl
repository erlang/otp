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

-module(ssl_cipher_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("ssl_internal.hrl").
-include("tls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").

-define(TIMEOUT, 600000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [aes_decipher_good, aes_decipher_good_tls11, aes_decipher_fail, aes_decipher_fail_tls11].

groups() ->
    [].

init_per_suite(Config) ->
    try crypto:start() of
	ok ->
	    Config
    catch _:_  ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
aes_decipher_good() ->
    [{doc,"Decipher a known cryptotext."}].

aes_decipher_good(Config) when is_list(Config) ->
    HashSz = 32,
    CipherState = #cipher_state{iv = <<59,201,85,117,188,206,224,136,5,109,46,70,104,79,4,9>>,
				key = <<72,196,247,97,62,213,222,109,210,204,217,186,172,184,197,148>>},
    Fragment = <<220,193,179,139,171,33,143,245,202,47,123,251,13,232,114,8,
		 190,162,74,31,186,227,119,155,94,74,119,79,169,193,240,160,
		 198,181,81,19,98,162,213,228,74,224,253,168,156,59,195,122,
		 108,101,107,242,20,15,169,150,163,107,101,94,93,104,241,165>>,
    Content = <<183,139,16,132,10,209,67,86,168,100,61,217,145,57,36,56, "HELLO\n">>,
    Mac = <<71,136,212,107,223,200,70,232,127,116,148,205,232,35,158,113,237,174,15,217,192,168,35,8,6,107,107,233,25,174,90,111>>,
    Version = {3,0},
    {Content, Mac, _} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version),
    Version1 = {3,1},
    {Content, Mac, _} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version1),
    ok.

%%--------------------------------------------------------------------

aes_decipher_good_tls11() ->
    [{doc,"Decipher a known TLS 1.1 cryptotext."}].

%% the fragment is actuall a TLS 1.1 record, with
%% Version = TLS 1.1, we get the correct NextIV in #cipher_state
aes_decipher_good_tls11(Config) when is_list(Config) ->
    HashSz = 32,
    CipherState = #cipher_state{iv = <<59,201,85,117,188,206,224,136,5,109,46,70,104,79,4,9>>,
				key = <<72,196,247,97,62,213,222,109,210,204,217,186,172,184,197,148>>},
    Fragment = <<220,193,179,139,171,33,143,245,202,47,123,251,13,232,114,8,
		 190,162,74,31,186,227,119,155,94,74,119,79,169,193,240,160,
		 198,181,81,19,98,162,213,228,74,224,253,168,156,59,195,122,
		 108,101,107,242,20,15,169,150,163,107,101,94,93,104,241,165>>,
    Content = <<"HELLO\n">>,
    NextIV = <<183,139,16,132,10,209,67,86,168,100,61,217,145,57,36,56>>,
    Mac = <<71,136,212,107,223,200,70,232,127,116,148,205,232,35,158,113,237,174,15,217,192,168,35,8,6,107,107,233,25,174,90,111>>,
    Version = {3,2},
    {Content, Mac, #cipher_state{iv = NextIV}} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version),
    Version1 = {3,2},
    {Content, Mac, #cipher_state{iv = NextIV}} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version1),
    ok.

%%--------------------------------------------------------------------

aes_decipher_fail() ->
    [{doc,"Decipher a known cryptotext."}].

%% same as above, last byte of key replaced
aes_decipher_fail(Config) when is_list(Config) ->
    HashSz = 32,
    CipherState = #cipher_state{iv = <<59,201,85,117,188,206,224,136,5,109,46,70,104,79,4,9>>,
				key = <<72,196,247,97,62,213,222,109,210,204,217,186,172,184,197,254>>},
    Fragment = <<220,193,179,139,171,33,143,245,202,47,123,251,13,232,114,8,
		 190,162,74,31,186,227,119,155,94,74,119,79,169,193,240,160,
		 198,181,81,19,98,162,213,228,74,224,253,168,156,59,195,122,
		 108,101,107,242,20,15,169,150,163,107,101,94,93,104,241,165>>,
    Version = {3,0},
    {Content, Mac, _} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version),
    32 = byte_size(Content),
    32 = byte_size(Mac),
    Version1 = {3,1},
    {Content1, Mac1, _} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version1),
    32 = byte_size(Content1),
    32 = byte_size(Mac1),
    ok.

%%--------------------------------------------------------------------

aes_decipher_fail_tls11() ->
    [{doc,"Decipher a known TLS 1.1 cryptotext."}].

%% same as above, last byte of key replaced
%% stricter padding checks in TLS 1.1 mean we get an alert instead
aes_decipher_fail_tls11(Config) when is_list(Config) ->
    HashSz = 32,
    CipherState = #cipher_state{iv = <<59,201,85,117,188,206,224,136,5,109,46,70,104,79,4,9>>,
				key = <<72,196,247,97,62,213,222,109,210,204,217,186,172,184,197,254>>},
    Fragment = <<220,193,179,139,171,33,143,245,202,47,123,251,13,232,114,8,
		 190,162,74,31,186,227,119,155,94,74,119,79,169,193,240,160,
		 198,181,81,19,98,162,213,228,74,224,253,168,156,59,195,122,
		 108,101,107,242,20,15,169,150,163,107,101,94,93,104,241,165>>,
    Version = {3,2},
    #alert{level = ?FATAL, description = ?BAD_RECORD_MAC} =
	ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version),
    Version1 = {3,3},
    #alert{level = ?FATAL, description = ?BAD_RECORD_MAC} =
	ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version1),
    ok.

%%--------------------------------------------------------------------
