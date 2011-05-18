%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010_2011. All Rights Reserved.
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
%% Purpose: Verify the reg component of the Diameter application
%%----------------------------------------------------------------------
%% 
-module(diameter_reg_test).

-export([
	 init_per_testcase/2, fin_per_testcase/2, 

	 all/0,
	 groups/0, 
	 init_per_suite/1, end_per_suite/1, 
	 suite_init/1, suite_fin/1, 
	 init_per_group/2, end_per_group/2

	 %% foo/1
	]).

-export([t/0, t/1]).

-include("diameter_test_lib.hrl").


t()     -> diameter_test_server:t(?MODULE).
t(Case) -> diameter_test_server:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    diameter_test_server:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    diameter_test_server:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite_init(X) -> init_per_suite(X).

init_per_suite(suite) -> [];
init_per_suite(doc) -> [];
init_per_suite(Config) when is_list(Config) ->
    Config.


suite_fin(X) -> end_per_suite(X).

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Test case example
%% 

%% foo(suite) ->
%%     [];
%% foo(doc) ->
%%     [];
%% foo(Config) when is_list(Config) ->
%%     ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


