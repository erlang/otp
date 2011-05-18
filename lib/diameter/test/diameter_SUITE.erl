%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(diameter_SUITE).

-export([
	 suite/0, 
	 all/0,
	 groups/0, 

	 init_per_testcase/2,
	 fin_per_testcase/2,

	 init_per_suite/1, 
	 end_per_suite/1, 

	 init_per_group/2, 
	 end_per_group/2, 

	 init/0
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

init() ->
    process_flag(trap_exit, true),
    ?FLUSH().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

suite() -> 
    [{ct_hooks, [{ts_install_cth, [{nodenames,1}]}]}].

all() ->
    [
     {group, app},
     {group, appup},
     {group, compiler}, 
     {group, config},
     {group, sync}, 
     {group, session},
     {group, stats},
     {group, reg},
     {group, peer}, 
     {group, tcp}
    ].

groups() ->
    [{app,      [], [{diameter_app_test,      all}]},
     {appup,    [], [{diameter_appup_test,    all}]},
     {compiler, [], [{diameter_compiler_test, all}]},
     {config,   [], [{diameter_config_test,   all}]}, 
     {sync,     [], [{diameter_sync_test,     all}]},
     {session,  [], [{diameter_session_test,  all}]}, 
     {stats,    [], [{diameter_stats_test,    all}]}, 
     {reg,      [], [{diameter_reg_test,      all}]}, 
     {peer,     [], [{diameter_peer_test,     all}]}, 
     {tcp,      [], [{diameter_tcp_test,      all}]}].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.
