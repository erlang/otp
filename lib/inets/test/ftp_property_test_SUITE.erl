%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%%   ct:run_test([{suite,"ftp_property_test_SUITE"}, {logdir,"/ldisk/OTP/LOG"}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%                       WARNING                               %%%
%%%                                                             %%%
%%% This is experimental code which may be changed or removed   %%%
%%%               anytime without any warning.                  %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ftp_property_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [prop_ftp_case].


init_per_suite(Config) ->
    inets:start(),
    ct_property_test:init_per_suite(Config).
    

%%%---- test case
prop_ftp_case(Config) ->
    ct_property_test:quickcheck(
      ftp_simple_client_server:prop_ftp(Config),
      Config
     ).
