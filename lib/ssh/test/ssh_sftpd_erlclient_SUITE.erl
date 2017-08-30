%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-module(ssh_sftpd_erlclient_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("ssh_test_lib.hrl").

-define(USER, "Alladin").
-define(PASSWD, "Sesame").
-define(SSH_MAX_PACKET_SIZE, 32768).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() -> 
    [close_file, 
     quit, 
     file_cb,
     root_dir, 
     list_dir_limited,
     ver6_basic].

groups() -> 
    [].

%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       begin
	   catch ssh:stop(),
	   DataDir = proplists:get_value(data_dir, Config),
	   PrivDir = proplists:get_value(priv_dir, Config),
	   FileAlt = filename:join(DataDir, "ssh_sftpd_file_alt.erl"),
	   c:c(FileAlt),
	   FileName = filename:join(DataDir, "test.txt"),
	   {ok, FileInfo} = file:read_file_info(FileName),
	   ok = file:write_file_info(FileName,
				     FileInfo#file_info{mode = 8#400}),
	   ssh_test_lib:setup_dsa(DataDir, PrivDir),
	   Config
       end).

end_per_suite(Config) -> 
    UserDir = filename:join(proplists:get_value(priv_dir, Config), nopubkey),
    file:del_dir(UserDir),
    SysDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_dsa(SysDir),
    ok.

%%--------------------------------------------------------------------

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.
%%--------------------------------------------------------------------

init_per_testcase(TestCase, Config) ->
    ssh:start(),
    PrivDir = proplists:get_value(priv_dir, Config),
    SystemDir = filename:join(PrivDir, system),

    Options =
	case atom_to_list(TestCase) of
	    "file_cb" ++ _ ->
		Spec =
		    ssh_sftpd:subsystem_spec([{file_handler,
					       ssh_sftpd_file_alt}]),
		[{system_dir, SystemDir},
		 {user_dir, PrivDir},
		 {subsystems, [Spec]}];
	    "root_dir" ->
		Privdir = proplists:get_value(priv_dir, Config),
		Root = filename:join(Privdir, root),
		file:make_dir(Root),
		Spec = ssh_sftpd:subsystem_spec([{root,Root}]),
		[{system_dir, SystemDir},
		 {user_dir, PrivDir},
		 {subsystems, [Spec]}];
	    "list_dir_limited" ->
		Spec =
		    ssh_sftpd:subsystem_spec([{max_files,1}]),
		[{system_dir, SystemDir},
		 {user_dir, PrivDir},
		 {subsystems, [Spec]}];
	    "ver6_basic" ->
		Spec =
		    ssh_sftpd:subsystem_spec([{sftpd_vsn, 6}]),
		[{system_dir, SystemDir},
		 {user_dir, PrivDir},
		 {subsystems, [Spec]}];
	    _ ->
		[{user_dir, PrivDir},
		 {system_dir, SystemDir}]
	end,

    {Sftpd, Host, Port} = ssh_test_lib:daemon(Options),

    {ok, ChannelPid, Connection} =
	ssh_sftp:start_channel(Host, Port,
			       [{silently_accept_hosts, true},
				{user_dir, PrivDir},
				{timeout, 30000}]),
    TmpConfig = lists:keydelete(sftp, 1, Config),
    NewConfig = lists:keydelete(sftpd, 1, TmpConfig),
    [{port, Port}, {sftp, {ChannelPid, Connection}}, {sftpd, Sftpd} | NewConfig].

end_per_testcase(_TestCase, Config) ->
    catch ssh_sftpd:stop(proplists:get_value(sftpd, Config)),
    {Sftp, Connection} = proplists:get_value(sftp, Config),
    catch ssh_sftp:stop_channel(Sftp),
    catch ssh:close(Connection),
    ssh:stop().

%%--------------------------------------------------------------------
%% Test cases starts here. -------------------------------------------
%%--------------------------------------------------------------------
close_file() ->
    [{doc, "Test that sftpd closes its fildescriptors after compleating the "
     "transfer OTP-6350"}].

close_file(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    FileName = filename:join(DataDir, "test.txt"),

    {Sftp, _} = proplists:get_value(sftp, Config),

    NumOfPorts = length(erlang:ports()),

    ct:log("Number of open ports:  ~p~n", [NumOfPorts]),

    {ok, <<_/binary>>} = ssh_sftp:read_file(Sftp, FileName),

    NumOfPorts = length(erlang:ports()).

%%--------------------------------------------------------------------

quit() ->
    [{doc, " When the sftp client ends the session the "
     "server will now behave correctly and not leave the "
     "client hanging. OTP-6349"}].

quit(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    FileName = filename:join(DataDir, "test.txt"),
    UserDir = proplists:get_value(priv_dir, Config), 
    Port = proplists:get_value(port, Config),

    {Sftp, _} = proplists:get_value(sftp, Config),

    {ok, <<_/binary>>} = ssh_sftp:read_file(Sftp, FileName),

    ok = ssh_sftp:stop_channel(Sftp),

    Host = ssh_test_lib:hostname(),

    timer:sleep(5000),
    {ok, NewSftp, _Conn} = ssh_sftp:start_channel(Host, Port,
						 [{silently_accept_hosts, true},
						  {pwdfun, fun(_,_) -> true end},
						  {user_dir, UserDir},
						  {user, ?USER}, {password, ?PASSWD}]),

    {ok, <<_/binary>>} = ssh_sftp:read_file(NewSftp, FileName),

    ok = ssh_sftp:stop_channel(NewSftp).

%%--------------------------------------------------------------------

file_cb() ->
    [{"Test that it is possible to change the callback module for"
      " the sftpds filehandling. OTP-6356"}].

file_cb(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir =  proplists:get_value(priv_dir, Config),
    FileName = filename:join(DataDir, "test.txt"),

    register(sftpd_file_alt_tester, self()),

    {Sftp, _} = proplists:get_value(sftp, Config),

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
    alt_file_handler_check(alt_del_dir).
%%--------------------------------------------------------------------

root_dir(Config) when is_list(Config) ->
    {Sftp, _} = proplists:get_value(sftp, Config),
    FileName = "test.txt",
    Bin =  <<"Test file for root dir option">>,
    ok = ssh_sftp:write_file(Sftp, FileName, Bin),
    {ok, Bin} = ssh_sftp:read_file(Sftp, FileName),
    {ok, Listing} =
	ssh_sftp:list_dir(Sftp, "."),
    ct:log("Listing: ~p~n", [Listing]).

%%--------------------------------------------------------------------
list_dir_limited(Config) when is_list(Config) ->
    {Sftp, _} = proplists:get_value(sftp, Config),
    {ok, Listing} =
	ssh_sftp:list_dir(Sftp, "."),
    ct:log("Listing: ~p~n", [Listing]).

%%--------------------------------------------------------------------
ver6_basic() ->
    [{doc, "Test some version 6 features"}].
ver6_basic(Config) when is_list(Config) ->
    PrivDir =  proplists:get_value(priv_dir, Config),
    NewDir = filename:join(PrivDir, "testdir2"),
    {Sftp, _} = proplists:get_value(sftp, Config),
    ok =  ssh_sftp:make_dir(Sftp, NewDir),
    %%Test file_is_a_directory
    {error, file_is_a_directory} = ssh_sftp:delete(Sftp, NewDir).
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
 
alt_file_handler_check(Msg) ->
    receive
	Msg ->
	    ok;
	Other ->
	    ct:fail({Msg, Other})
    after 10000 ->
	    ct:fail("Not alt file handler")
    end.
