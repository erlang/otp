%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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

-include("test_server.hrl").
-include("test_server_line.hrl").

%% Test server specific exports
-export([all/1]).
% -export([init_per_testcase/2, end_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

%% Test cases must be exported.
-export([solaris8_test/1,
	 solaris9_test/1,
	 solaris10_test/1,
	 linux_x86_test/1,
	 linux_ppc_test/1,
	 macosx_x86_test/1,
	 macosx_ppc_test/1,
	 openbsd_test/1,
	 freebsd_test/1,
	 netbsd_test/1,
	 windows_xp_test/1,
	 windows_2003_server_test/1,
	 ticket_tests/1]).

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
all(doc) ->
    ["Test the ftp client in the inets application."];
all(suite) ->
    [
     solaris8_test,
     solaris9_test,
     solaris10_test,
     linux_x86_test,
     linux_ppc_test,
     macosx_x86_test,
     macosx_ppc_test,
     openbsd_test,
     freebsd_test,
     netbsd_test,
     windows_xp_test,
     windows_2003_server_test,
     ticket_tests
    ].

solaris8_test(suite) ->
    [{ftp_solaris8_sparc_test,all}].
solaris9_test(suite) ->
    [{ftp_solaris9_sparc_test,all}].
solaris10_test(suite) ->
    [{ftp_solaris10_sparc_test,all}, {ftp_solaris10_x86_test,all}].
linux_x86_test(suite) ->
    [{ftp_linux_x86_test,all}].
linux_ppc_test(suite) ->
    [{ftp_linux_ppc_test,all}].
macosx_x86_test(suite) ->
    [{ftp_macosx_x86_test,all}].
macosx_ppc_test(suite) ->
    [{ftp_macosx_ppc_test,all}].
openbsd_test(suite) ->
    [{ftp_openbsd_x86_test,all}].
freebsd_test(suite) ->
    [{ftp_freebsd_x86_test,all}].
netbsd_test(suite) ->
    [{ftp_netbsd_x86_test,all}].
windows_xp_test(suite) ->
    [{ftp_windows_xp_test,all}].
windows_2003_server_test(suite) ->
    [{ftp_windows_2003_server_test,all}].

ticket_tests(suite) ->
    [{ftp_ticket_test, all}].

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
