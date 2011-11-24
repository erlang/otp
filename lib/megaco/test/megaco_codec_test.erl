%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
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
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_codec_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).

init() ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all() -> 
    [{group, codec}].

groups() -> 
    [{codec, [],
      [{megaco_codec_mini_test, all},
       {megaco_codec_v1_test, all},
       {megaco_codec_v2_test, all},
       {megaco_codec_prev3a_test, all},
       {megaco_codec_prev3b_test, all},
       {megaco_codec_prev3c_test, all},
       {megaco_codec_v3_test, all}]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



