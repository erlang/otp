%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").

-define(TIMEOUT, 600000).

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initialization before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    try crypto:start() of
	ok ->
	    Config
    catch _:_  ->
	    {skip, "Crypto did not start"}
    end.
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initialization before each test case
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ssl_test_lib:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Dog = ?config(watchdog, Config),
    case Dog of 
	undefined ->
	    ok;
	_ ->
	    test_server:timetrap_cancel(Dog)
    end.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [aes_decipher_good, aes_decipher_fail].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Test cases starts here.
%%--------------------------------------------------------------------
aes_decipher_good(doc) -> 
    ["Decipher a known cryptotext."];

aes_decipher_good(suite) -> 
    [];

aes_decipher_good(Config) when is_list(Config) ->
    HashSz = 32,
    CipherState = #cipher_state{iv = <<59,201,85,117,188,206,224,136,5,109,46,70,104,79,4,9>>,
				key = <<72,196,247,97,62,213,222,109,210,204,217,186,172,184,197,148>>},
    Fragment = <<220,193,179,139,171,33,143,245,202,47,123,251,13,232,114,8,
		 190,162,74,31,186,227,119,155,94,74,119,79,169,193,240,160,
		 198,181,81,19,98,162,213,228,74,224,253,168,156,59,195,122,
		 108,101,107,242,20,15,169,150,163,107,101,94,93,104,241,165>>,
    Version = {3,3},
    Content = <<183,139,16,132,10,209,67,86,168,100,61,217,145,57,36,56,72,69,76,76,79,10>>,
    Mac = <<71,136,212,107,223,200,70,232,127,116,148,205,232,35,158,113,237,174,15,217,192,168,35,8,6,107,107,233,25,174,90,111>>,
    {Content, Mac, _} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version),
    ok.

%%--------------------------------------------------------------------

aes_decipher_fail(doc) -> 
    ["Decipher a known cryptotext."];

aes_decipher_fail(suite) -> 
    [];

%% same as above, last byte of key replaced
aes_decipher_fail(Config) when is_list(Config) ->
    HashSz = 32,
    CipherState = #cipher_state{iv = <<59,201,85,117,188,206,224,136,5,109,46,70,104,79,4,9>>,
				key = <<72,196,247,97,62,213,222,109,210,204,217,186,172,184,197,254>>},
    Fragment = <<220,193,179,139,171,33,143,245,202,47,123,251,13,232,114,8,
		 190,162,74,31,186,227,119,155,94,74,119,79,169,193,240,160,
		 198,181,81,19,98,162,213,228,74,224,253,168,156,59,195,122,
		 108,101,107,242,20,15,169,150,163,107,101,94,93,104,241,165>>,
    Version = {3,3},
    {Content, Mac, _} = ssl_cipher:decipher(?AES, HashSz, CipherState, Fragment, Version),
    32 = byte_size(Content),
    32 = byte_size(Mac),
    ok.

%%--------------------------------------------------------------------
