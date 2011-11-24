%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
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

-module(ftp_openbsd_x86_test).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(LIB_MOD,ftp_suite_lib).
-define(CASE_WRAPPER(_A_,_B_,_C_),?LIB_MOD:wrapper(_A_,_B_,_C_)).
-define(PLATFORM,"Openbsd x86 ").

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {File, NewFile} = ?LIB_MOD:test_filenames(),
    NewConfig = [{file, File}, {new_file, NewFile} | Config],
    ?LIB_MOD:ftpd_init(openbsd_x86, NewConfig).

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    ?LIB_MOD:ftpd_fin(Config).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initiation before each test case
%%--------------------------------------------------------------------
init_per_testcase(Case, Config) ->
    ftp_suite_lib:init_per_testcase(Case, Config).
%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(Case, Config) ->
    ftp_suite_lib:end_per_testcase(Case, Config).

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
all() -> 
    [open, open_port, {group, passive}, {group, active},
     api_missuse, not_owner, {group, progress_report}].

groups() -> 
    [{passive, [], ftp_suite_lib:passive(suite)},
     {active, [], ftp_suite_lib:active(suite)},
     {progress_report, [],
      ftp_suite_lib:progress_report(suite)}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Test cases starts here.
%%--------------------------------------------------------------------

open(X) -> ?CASE_WRAPPER(?PLATFORM,X,fun ?LIB_MOD:open/1).
open_port(X) -> ?CASE_WRAPPER(?PLATFORM,X,fun ?LIB_MOD:open_port/1).
api_missuse(X) -> ?CASE_WRAPPER(?PLATFORM,X,fun ?LIB_MOD:api_missuse/1).
not_owner(X) -> ?CASE_WRAPPER(?PLATFORM,X,fun ?LIB_MOD:not_owner/1).

passive_user(X) -> ?LIB_MOD:passive_user(X).
passive_pwd(X) -> ?LIB_MOD:passive_pwd(X).
passive_cd(X) -> ?LIB_MOD:passive_cd(X).
passive_lcd(X) -> ?LIB_MOD:passive_lcd(X).
passive_ls(X) -> ?LIB_MOD:passive_ls(X).
passive_nlist(X) -> ?LIB_MOD:passive_nlist(X).
passive_rename(X) -> ?LIB_MOD:passive_rename(X).
passive_delete(X) -> ?LIB_MOD:passive_delete(X).
passive_mkdir(X) -> ?LIB_MOD:passive_mkdir(X).
passive_send(X) -> ?LIB_MOD:passive_send(X).
passive_send_bin(X) -> ?LIB_MOD:passive_send_bin(X).
passive_send_chunk(X) -> ?LIB_MOD:passive_send_chunk(X).
passive_append(X) -> ?LIB_MOD:passive_append(X).
passive_append_bin(X) -> ?LIB_MOD:passive_append_bin(X).
passive_append_chunk(X) -> ?LIB_MOD:passive_append_chunk(X).
passive_recv(X) -> ?LIB_MOD:passive_recv(X).
passive_recv_bin(X) -> ?LIB_MOD:passive_recv_bin(X).
passive_recv_chunk(X) -> ?LIB_MOD:passive_recv_chunk(X).
passive_type(X) -> ?LIB_MOD:passive_type(X).
passive_quote(X) -> ?LIB_MOD:passive_quote(X).
passive_ip_v6_disabled(X) -> ?LIB_MOD:passive_ip_v6_disabled(X).
active_user(X) -> ?LIB_MOD:active_user(X). 
active_pwd(X) -> ?LIB_MOD:active_pwd(X). 
active_cd(X) -> ?LIB_MOD:active_cd(X).
active_lcd(X) -> ?LIB_MOD:active_lcd(X). 
active_ls(X) -> ?LIB_MOD:active_ls(X). 
active_nlist(X) -> ?LIB_MOD:active_nlist(X). 
active_rename(X) -> ?LIB_MOD:active_rename(X). 
active_delete(X) -> ?LIB_MOD:active_delete(X). 
active_mkdir(X) -> ?LIB_MOD:active_mkdir(X). 
active_send(X) -> ?LIB_MOD:active_send(X). 
active_send_bin(X) -> ?LIB_MOD:active_send_bin(X). 
active_send_chunk(X) -> ?LIB_MOD:active_send_chunk(X). 
active_append(X) -> ?LIB_MOD:active_append(X). 
active_append_bin(X) -> ?LIB_MOD:active_append_bin(X). 
active_append_chunk(X) -> ?LIB_MOD:active_append_chunk(X). 
active_recv(X) -> ?LIB_MOD:active_recv(X). 
active_recv_bin(X) -> ?LIB_MOD:active_recv_bin(X). 
active_recv_chunk(X) -> ?LIB_MOD:active_recv_chunk(X). 
active_type(X) -> ?LIB_MOD:active_type(X). 
active_quote(X) -> ?LIB_MOD:active_quote(X). 
active_ip_v6_disabled(X) -> ?LIB_MOD:active_ip_v6_disabled(X).
progress_report_send(X) -> ?LIB_MOD:progress_report_send(X).
progress_report_recv(X) -> ?LIB_MOD:progress_report_recv(X).
