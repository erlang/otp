%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

-module(ftp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
% -export([init_per_testcase/2, end_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

-define(FTP_USER, "anonymous").
-define(FTP_PASS, passwd()).
-define(FTP_PORT, 21).

-define(BAD_HOST, "badhostname").
-define(BAD_USER, "baduser").
-define(BAD_DIR,  "baddirectory").

-ifdef(ftp_debug_client).
-define(ftp_open(Host, Flags), do_ftp_open(Host, [debug] ++ Flags)).
-else.
-ifdef(ftp_trace_client).
-define(ftp_open(Host, Flags), do_ftp_open(Host, [trace] ++ Flags)).
-else.
-define(ftp_open(Host, Flags), do_ftp_open(Host, [verbose] ++ Flags)).
-endif.
-endif.


%%--------------------------------------------------------------------
%% all(Arg) -> [Doc] | [Case] | {skip, Comment}
%% Arg - doc | suite
%% Doc - string()
%% Case - atom() 
%%	Name of a test case function. 
%% Comment - string()
%% Description: Returns documentation/test cases in this test suite
%%		or a skip tuple if the platform is not supported.  
%%--------------------------------------------------------------------
suite() -> [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     {group, solaris8_test},   
     {group, solaris9_test},
     {group, solaris10_test}, 
     {group, linux_x86_test},
     {group, linux_ppc_test}, 
     {group, macosx_x86_test},
     {group, macosx_ppc_test}, 
     {group, openbsd_test},
     {group, freebsd_test}, 
     {group, netbsd_test},
     {group, windows_xp_test},
     {group, windows_2003_server_test},
     {group, ticket_tests}
    ].

groups() -> 
    [
     {solaris8_test,            [], [{ftp_solaris8_sparc_test, all}]},
     {solaris9_test,            [], [{ftp_solaris9_sparc_test, all}]},
     {solaris10_test,           [], [{ftp_solaris10_sparc_test, all},
				     {ftp_solaris10_x86_test, all}]},
     {linux_x86_test,           [], [{ftp_linux_x86_test, all}]},
     {linux_ppc_test,           [], [{ftp_linux_ppc_test, all}]},
     {macosx_x86_test,          [], [{ftp_macosx_x86_test, all}]},
     {macosx_ppc_test,          [], [{ftp_macosx_ppc_test, all}]},
     {openbsd_test,             [], [{ftp_openbsd_x86_test, all}]},
     {freebsd_test,             [], [{ftp_freebsd_x86_test, all}]},
     {netbsd_test,              [], [{ftp_netbsd_x86_test, all}]},
     {windows_xp_test,          [], [{ftp_windows_xp_test, all}]},
     {windows_2003_server_test, [], [{ftp_windows_2003_server_test, all}]},
     {ticket_tests,             [], [{ftp_ticket_test, all}]}
    ].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.




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
    inets:start(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    inets:stop(),
    ok.
