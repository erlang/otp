%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(blowfish_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TIMEOUT, 120000). % 2 min

-define(KEY, to_bin("0123456789ABCDEFF0E1D2C3B4A59687")).
-define(IVEC, to_bin("FEDCBA9876543210")).
%% "7654321 Now is the time for " (includes trailing '\0')
-define(DATA, to_bin("37363534333231204E6F77206973207468652074696D6520666F722000")).
-define(DATA_PADDED, to_bin("37363534333231204E6F77206973207468652074696D6520666F722000000000")).

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
    case catch crypto:start() of
	ok ->
	    Config;
	_Else ->
	    {skip,"Could not start crypto!"}
    end.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    crypto:stop().

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
    Dog = test_server:timetrap(?TIMEOUT),
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
[ecb, cbc, cfb64, ofb64].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


%% Test cases start here.
%%--------------------------------------------------------------------

ecb_test(KeyBytes, ClearBytes, CipherBytes) ->
    {Key, Clear, Cipher} =
	{to_bin(KeyBytes), to_bin(ClearBytes), to_bin(CipherBytes)},
    ?line m(crypto:blowfish_ecb_encrypt(Key, Clear), Cipher),
    true.

ecb(doc) ->
    "Test that ECB mode is OK";
ecb(suite) ->
    [];
ecb(Config) when is_list(Config) ->
	true = ecb_test("0000000000000000", "0000000000000000", "4EF997456198DD78"),
	true = ecb_test("FFFFFFFFFFFFFFFF", "FFFFFFFFFFFFFFFF", "51866FD5B85ECB8A"),
	true = ecb_test("3000000000000000", "1000000000000001", "7D856F9A613063F2"),
	true = ecb_test("1111111111111111", "1111111111111111", "2466DD878B963C9D"),
	true = ecb_test("0123456789ABCDEF", "1111111111111111", "61F9C3802281B096"),
	true = ecb_test("1111111111111111", "0123456789ABCDEF", "7D0CC630AFDA1EC7"),
	true = ecb_test("0000000000000000", "0000000000000000", "4EF997456198DD78"),
	true = ecb_test("FEDCBA9876543210", "0123456789ABCDEF", "0ACEAB0FC6A0A28D"),
	true = ecb_test("7CA110454A1A6E57", "01A1D6D039776742", "59C68245EB05282B"),
	true = ecb_test("0131D9619DC1376E", "5CD54CA83DEF57DA", "B1B8CC0B250F09A0"),
	true = ecb_test("07A1133E4A0B2686", "0248D43806F67172", "1730E5778BEA1DA4"),
	true = ecb_test("3849674C2602319E", "51454B582DDF440A", "A25E7856CF2651EB"),
	true = ecb_test("04B915BA43FEB5B6", "42FD443059577FA2", "353882B109CE8F1A"),
	true = ecb_test("0113B970FD34F2CE", "059B5E0851CF143A", "48F4D0884C379918"),
	true = ecb_test("0170F175468FB5E6", "0756D8E0774761D2", "432193B78951FC98"),
	true = ecb_test("43297FAD38E373FE", "762514B829BF486A", "13F04154D69D1AE5"),
	true = ecb_test("07A7137045DA2A16", "3BDD119049372802", "2EEDDA93FFD39C79"),
	true = ecb_test("04689104C2FD3B2F", "26955F6835AF609A", "D887E0393C2DA6E3"),
	true = ecb_test("37D06BB516CB7546", "164D5E404F275232", "5F99D04F5B163969"),
	true = ecb_test("1F08260D1AC2465E", "6B056E18759F5CCA", "4A057A3B24D3977B"),
	true = ecb_test("584023641ABA6176", "004BD6EF09176062", "452031C1E4FADA8E"),
	true = ecb_test("025816164629B007", "480D39006EE762F2", "7555AE39F59B87BD"),
	true = ecb_test("49793EBC79B3258F", "437540C8698F3CFA", "53C55F9CB49FC019"),
	true = ecb_test("4FB05E1515AB73A7", "072D43A077075292", "7A8E7BFA937E89A3"),
	true = ecb_test("49E95D6D4CA229BF", "02FE55778117F12A", "CF9C5D7A4986ADB5"),
	true = ecb_test("018310DC409B26D6", "1D9D5C5018F728C2", "D1ABB290658BC778"),
	true = ecb_test("1C587F1C13924FEF", "305532286D6F295A", "55CB3774D13EF201"),
	true = ecb_test("0101010101010101", "0123456789ABCDEF", "FA34EC4847B268B2"),
	true = ecb_test("1F1F1F1F0E0E0E0E", "0123456789ABCDEF", "A790795108EA3CAE"),
	true = ecb_test("E0FEE0FEF1FEF1FE", "0123456789ABCDEF", "C39E072D9FAC631D"),
	true = ecb_test("0000000000000000", "FFFFFFFFFFFFFFFF", "014933E0CDAFF6E4"),
	true = ecb_test("FFFFFFFFFFFFFFFF", "0000000000000000", "F21E9A77B71C49BC"),
	true = ecb_test("0123456789ABCDEF", "0000000000000000", "245946885754369A"),
	true = ecb_test("FEDCBA9876543210", "FFFFFFFFFFFFFFFF", "6B5C5A9C5D9E0A5A"),
	ok.

cbc(doc) ->
    "Test that CBC mode is OK";
cbc(suite) ->
    [];
cbc(Config) when is_list(Config) ->
	true = crypto:blowfish_cbc_encrypt(?KEY, ?IVEC, ?DATA_PADDED) =:=
		to_bin("6B77B4D63006DEE605B156E27403979358DEB9E7154616D959F1652BD5FF92CC"),
	ok.

cfb64(doc) ->
    "Test that CFB64 mode is OK";
cfb64(suite) ->
    [];
cfb64(Config) when is_list(Config) ->
	true = crypto:blowfish_cfb64_encrypt(?KEY, ?IVEC, ?DATA) =:=
		to_bin("E73214A2822139CAF26ECF6D2EB9E76E3DA3DE04D1517200519D57A6C3"),
	ok.

ofb64(doc) ->
    "Test that OFB64 mode is OK";
ofb64(suite) ->
    [];
ofb64(Config) when is_list(Config) ->
	true = crypto:blowfish_ofb64_encrypt(?KEY, ?IVEC, ?DATA) =:=
		to_bin("E73214A2822139CA62B343CC5B65587310DD908D0C241B2263C2CF80DA"),
	ok.

%% Helper functions

%% Convert a hexadecimal string to a binary.
-spec(to_bin(L::string()) -> binary()).
to_bin(L) ->
    to_bin(L, []).

%% @spec dehex(char()) -> integer()
%% @doc Convert a hex digit to its integer value.
-spec(dehex(char()) -> integer()).
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

-spec(to_bin(L::string(), list()) -> binary()).
to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

m(X,X) -> ok.
