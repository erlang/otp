%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2014. All Rights Reserved.
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
