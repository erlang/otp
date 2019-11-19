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

%%% Run like this:
%%%  ct:run_test([{suite,"ssh_property_test_SUITE"}, {logdir,"/ldisk/OTP/LOG"}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%                       WARNING                               %%%
%%%                                                             %%%
%%% This is experimental code which may be changed or removed   %%%
%%%               anytime without any warning.                  %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ssh_property_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ssh_test_lib.hrl").


all() -> [{group, messages},
	  client_sends_info_timing,
	  {group, client_server}
	 ].

groups() ->
    [{messages, [], [decode,
		     decode_encode]},
     {client_server, [], [client_server_sequential,
                          client_server_parallel
			  %% client_server_parallel_multi
                         ]}
    ].


%%% First prepare Config and compile the property tests for the found tool:
init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       ct_property_test:init_per_suite(Config)
      ).

end_per_suite(_Config) ->
    ok.

%%% One group in this suite happens to support only QuickCheck, so skip it
%%% if we run proper.
init_per_group(client_server, Config) ->
    case proplists:get_value(property_test_tool,Config) of
	proper -> Config;
	X -> {skip, lists:concat([X," is not supported"])}
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_TestCase, Config) -> Config.
    
end_per_testcase(_TestCase, Config) -> Config.

%%%================================================================
%%% Test suites
%%%
decode(Config) ->
    ct_property_test:quickcheck(
      ssh_eqc_encode_decode:prop_ssh_decode(),
      Config
     ).

decode_encode(Config) ->
    ct_property_test:quickcheck(
      ssh_eqc_encode_decode:prop_ssh_decode_encode(),
      Config
     ).

client_server_sequential(Config) ->
    ct_property_test:quickcheck(
      ssh_eqc_client_server:prop_seq(Config),
      Config
     ).

client_server_parallel(Config) ->
    ct_property_test:quickcheck(
      ssh_eqc_client_server:prop_parallel(Config),
      Config
     ).

client_server_parallel_multi(Config) ->
    ct_property_test:quickcheck(
      ssh_eqc_client_server:prop_parallel_multi(Config),
      Config
     ).

client_sends_info_timing(Config) ->
    ct_property_test:quickcheck(
      ssh_eqc_client_info_timing:prop_seq(Config),
      Config
     ).
