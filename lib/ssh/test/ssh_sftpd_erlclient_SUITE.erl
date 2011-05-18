%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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
-module(ssh_sftpd_erlclient_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").

-include_lib("kernel/include/file.hrl").

-define(SSHD_PORT, 9999).
-define(USER, "Alladin").
-define(PASSWD, "Sesame").
-define(SSH_MAX_PACKET_SIZE, 32768).

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
    catch ssh:stop(),
    case catch crypto:start() of
	ok ->
	    DataDir = ?config(data_dir, Config),
	    FileAlt = filename:join(DataDir, "ssh_sftpd_file_alt.erl"),
	    c:c(FileAlt),
	    FileName = filename:join(DataDir, "test.txt"),
	    {ok, FileInfo} = file:read_file_info(FileName),
	    ok = file:write_file_info(FileName,
				      FileInfo#file_info{mode = 8#400}),
	    ssh_test_lib:make_dsa_files(Config),
	    Config;
	_Else ->
	    {skip,"Could not start ssh!"}
    end.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    crypto:stop(),
    ok.

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
init_per_testcase(TestCase, Config) ->
    ssh:start(),
    DataDir = ?config(data_dir, Config),

    Options =
	case atom_to_list(TestCase) of
	    "file_cb" ++ _ ->
		Spec =
		    ssh_sftpd:subsystem_spec([{file_handler,
					       ssh_sftpd_file_alt}]),
		[{user_passwords,[{?USER, ?PASSWD}]},
		 {pwdfun, fun(_,_) -> true end},
		 {system_dir, DataDir},
		 {user_dir, DataDir},
		 {subsystems, [Spec]}];
	    "root_dir" ->
		Privdir = ?config(priv_dir, Config),
		Root = filename:join(Privdir, root),
		file:make_dir(Root),
		Spec = ssh_sftpd:subsystem_spec([{root,Root}]),
		[{user_passwords,[{?USER, ?PASSWD}]},
		 {pwdfun, fun(_,_) -> true end},
		 {system_dir, DataDir},
		 {user_dir, DataDir},
		 {subsystems, [Spec]}];
	    "list_dir_limited" ->
		Spec =
		    ssh_sftpd:subsystem_spec([{max_files,1}]),
		[{user_passwords,[{?USER, ?PASSWD}]},
		 {pwdfun, fun(_,_) -> true end},
		 {system_dir, DataDir},
		 {user_dir, DataDir},
		 {subsystems, [Spec]}];

	    _ ->
		[{user_passwords,[{?USER, ?PASSWD}]},
		 {pwdfun, fun(_,_) -> true end},
		 {user_dir, DataDir},
		 {system_dir, DataDir}]
	end,

    {Sftpd, Host, _Port} = ssh_test_lib:daemon(any, ?SSHD_PORT, Options),

    {ok, ChannelPid, Connection} =
	ssh_sftp:start_channel(Host, ?SSHD_PORT,
			       [{silently_accept_hosts, true},
				{user, ?USER}, {password, ?PASSWD}, 
				{pwdfun, fun(_,_) -> true end},
				{system_dir, DataDir},
				{user_dir, DataDir},
				{timeout, 30000}]),
    TmpConfig = lists:keydelete(sftp, 1, Config),
    NewConfig = lists:keydelete(sftpd, 1, TmpConfig),
    [{sftp, {ChannelPid, Connection}}, {sftpd, Sftpd} | NewConfig].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    catch ssh_sftpd:stop(?config(sftpd, Config)),
    {Sftp, Connection} = ?config(sftp, Config),
    catch ssh_sftp:stop_channel(Sftp),
    catch ssh:close(Connection),
    ssh:stop(),
    ok.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
all() -> 
    [close_file_OTP_6350, quit_OTP_6349, file_cb_OTP_6356,
     root_dir, list_dir_limited].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%% Test cases starts here.
%%--------------------------------------------------------------------
close_file_OTP_6350(doc) ->
    ["Test that sftpd closes its fildescriptors after compleating the "
     "transfer"];

close_file_OTP_6350(suite) ->
    [];

close_file_OTP_6350(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    FileName = filename:join(DataDir, "test.txt"),

    {Sftp, _} = ?config(sftp, Config),

    NumOfPorts = length(erlang:ports()),

    test_server:format("Number of open ports:  ~p~n", [NumOfPorts]),

    {ok, <<_/binary>>} = ssh_sftp:read_file(Sftp, FileName),

    NumOfPorts = length(erlang:ports()),

    test_server:format("Number of open ports:  ~p~n",
		       [length(erlang:ports())]),

    ok.

%%--------------------------------------------------------------------

quit_OTP_6349(doc) ->
    [" When the sftp client ends the session the "
     "server will now behave correctly and not leave the "
     "client hanging."];

quit_OTP_6349(suite) ->
    [];

quit_OTP_6349(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    FileName = filename:join(DataDir, "test.txt"),

    {Sftp, _} = ?config(sftp, Config),

    {ok, <<_/binary>>} = ssh_sftp:read_file(Sftp, FileName),

    ok = ssh_sftp:stop_channel(Sftp),

    Host = ssh_test_lib:hostname(),

    timer:sleep(5000),
    {ok, NewSftp, _Conn} = ssh_sftp:start_channel(Host, ?SSHD_PORT,
						 [{silently_accept_hosts, true},
						  {pwdfun, fun(_,_) -> true end},
						  {system_dir, DataDir},
						  {user_dir, DataDir},
						  {user, ?USER}, {password, ?PASSWD}]),

    {ok, <<_/binary>>} = ssh_sftp:read_file(NewSftp, FileName),

    ok = ssh_sftp:stop_channel(NewSftp),
    ok.

%%--------------------------------------------------------------------

file_cb_OTP_6356(doc) ->
    ["Test that it is possible to change the callback module for"
    " the sftpds filehandling."];

file_cb_OTP_6356(suite) ->
    [];

file_cb_OTP_6356(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(DataDir, "test.txt"),

    register(sftpd_file_alt_tester, self()),

    {Sftp, _} = ?config(sftp, Config),

    {ok, Bin} = ssh_sftp:read_file(Sftp, FileName),
    alt_file_handler_check(alt_open),
    alt_file_handler_check(alt_read_file_info),
    alt_file_handler_check(alt_position),
    alt_file_handler_check(alt_read),
    alt_file_handler_check(alt_position),
    alt_file_handler_check(alt_read),
    alt_file_handler_check(alt_close),


    NewFileName = filename:join(PrivDir, "test.txt"),
    ok = ssh_sftp:write_file(Sftp, NewFileName, Bin),
    alt_file_handler_check(alt_open),
    alt_file_handler_check(alt_read_file_info),
    alt_file_handler_check(alt_position),
    alt_file_handler_check(alt_write),
    alt_file_handler_check(alt_close),

    ReFileName = filename:join(PrivDir, "test1.txt"),
    ok = ssh_sftp:rename(Sftp, NewFileName, ReFileName),
    alt_file_handler_check(alt_rename),

    ok = ssh_sftp:delete(Sftp, ReFileName),
    alt_file_handler_check(alt_delete),

    NewDir = filename:join(PrivDir, "testdir"),
    ok =  ssh_sftp:make_dir(Sftp, NewDir),
    alt_file_handler_check(alt_make_dir),

    ok = ssh_sftp:del_dir(Sftp, NewDir),
    alt_file_handler_check(alt_read_link_info),
    alt_file_handler_check(alt_write_file_info),
    alt_file_handler_check(alt_del_dir),
    ok.

root_dir(doc) ->
    [""];
root_dir(suite) ->
    [];
root_dir(Config) when is_list(Config) ->
    {Sftp, _} = ?config(sftp, Config),
    FileName = "test.txt",
    Bin =  <<"Test file for root dir option">>,
    ok = ssh_sftp:write_file(Sftp, FileName, Bin),
    {ok, Bin} = ssh_sftp:read_file(Sftp, FileName),
    {ok, Listing} =
	ssh_sftp:list_dir(Sftp, "."),
    test_server:format("Listing: ~p~n", [Listing]),
    ok.

list_dir_limited(doc) ->
    [""];
list_dir_limited(suite) ->
    [];
list_dir_limited(Config) when is_list(Config) ->
    {Sftp, _} = ?config(sftp, Config),
    {ok, Listing} =
	ssh_sftp:list_dir(Sftp, "."),
    test_server:format("Listing: ~p~n", [Listing]),
    ok.

alt_file_handler_check(Msg) ->
    receive
	Msg ->
	    ok;
	Other ->
	    test_server:fail({Msg, Other})
    after 10000 ->
	    test_server:fail("Not alt file handler")
    end.
