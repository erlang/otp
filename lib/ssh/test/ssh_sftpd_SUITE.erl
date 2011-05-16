%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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
-module(ssh_sftpd_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").
-include("ssh_xfer.hrl").
-include("ssh.hrl").

-include_lib("kernel/include/file.hrl").

-define(SFPD_PORT, 9999).
-define(USER, "Alladin").
-define(PASSWD, "Sesame").
-define(XFER_PACKET_SIZE, 32768).
-define(XFER_WINDOW_SIZE, 4*?XFER_PACKET_SIZE).
-define(TIMEOUT, 10000).
-define(REG_ATTERS, <<0,0,0,0,1>>).
-define(UNIX_EPOCH,  62167219200).

-define(is_set(F, Bits),
	((F) band (Bits)) == (F)).

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
    case {catch ssh:stop(),catch crypto:start()} of
	{ok,ok} ->
	    ssh_test_lib:make_dsa_files(Config),
	    Config;
	{ok,_} ->
	    {skip,"Could not start ssh!"};
	{_,ok} ->
	    {skip,"Could not start crypto!"};
	{_,_} ->
	    {skip,"Could not start crypto and ssh!"}
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
    prep(Config),
    SysDir = ?config(data_dir, Config),
    {ok, Sftpd} =
	ssh_sftpd:listen(?SFPD_PORT, [{system_dir, SysDir},
				      {user_passwords,[{?USER, ?PASSWD}]},
				      {pwdfun, fun(_,_) -> true end}]),
    
    Cm = ssh_test_lib:connect(?SFPD_PORT,
			      [{system_dir, SysDir},
			       {user_dir, SysDir},
			       {user, ?USER}, {password, ?PASSWD},
			       {user_interaction, false},
			       {silently_accept_hosts, true},
			       {pwdfun, fun(_,_) -> true end}]),
    {ok, Channel} =
	ssh_connection:session_channel(Cm, ?XFER_WINDOW_SIZE,
				       ?XFER_PACKET_SIZE, ?TIMEOUT),
    
    success = ssh_connection:subsystem(Cm, Channel, "sftp", ?TIMEOUT),

    ProtocolVer = case atom_to_list(TestCase) of
		      "ver3_" ++ _ ->
			  3;
		      _ ->
			  ?SSH_SFTP_PROTOCOL_VERSION
		  end,

    Data = <<?UINT32(ProtocolVer)>> ,

    Size = 1 + size(Data),

    ssh_connection:send(Cm, Channel, << ?UINT32(Size),
				      ?SSH_FXP_INIT, Data/binary >>),

    {ok, <<?SSH_FXP_VERSION, ?UINT32(Version), _Ext/binary>>, _}
	= reply(Cm, Channel),

    test_server:format("Client: ~p Server ~p~n", [ProtocolVer, Version]),

    [{sftp, {Cm, Channel}}, {sftpd, Sftpd }| Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    ssh_sftpd:stop(?config(sftpd, Config)),
    {Cm, Channel} = ?config(sftp, Config),
    ssh_connection:close(Cm, Channel),
    ssh:close(Cm),
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
    [open_close_file, open_close_dir, read_file, read_dir,
     write_file, rename_file, mk_rm_dir, remove_file,
     real_path, retrieve_attributes, set_attributes, links,
     ver3_rename_OTP_6352, seq10670, sshd_read_file].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


%% Test cases starts here.
%%--------------------------------------------------------------------
open_close_file(doc) ->
    ["Test SSH_FXP_OPEN and SSH_FXP_CLOSE commands"];
open_close_file(suite) ->
    [];
open_close_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    {Cm, Channel} = ?config(sftp, Config),
    ReqId = 0,

    {ok, <<?SSH_FXP_HANDLE, ?UINT32(ReqId), Handle/binary>>, _} =
	open_file(FileName, Cm, Channel, ReqId,
		  ?ACE4_READ_DATA  bor ?ACE4_READ_ATTRIBUTES,
		  ?SSH_FXF_OPEN_EXISTING),

    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	  ?UINT32(?SSH_FX_OK), _/binary>>, _} = close(Handle, ReqId,
						      Cm, Channel),
    NewReqId = ReqId + 1,
    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	  ?UINT32(?SSH_FX_INVALID_HANDLE), _/binary>>, _} =
	close(Handle, ReqId, Cm, Channel),

    NewReqId1 = NewReqId + 1,
    %%  {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId),  % Ver 6 we have 5
    %% 	   ?UINT32(?SSH_FX_FILE_IS_A_DIRECTORY), _/binary>>, _} =
    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId1),
	  ?UINT32(?SSH_FX_FAILURE), _/binary>>, _} =
	open_file(PrivDir, Cm, Channel, NewReqId1,
		  ?ACE4_READ_DATA  bor ?ACE4_READ_ATTRIBUTES,
		  ?SSH_FXF_OPEN_EXISTING),

    ok.

%%--------------------------------------------------------------------
open_close_dir(doc) ->
    ["Test SSH_FXP_OPENDIR and SSH_FXP_CLOSE commands"];
open_close_dir(suite) ->
    [];
open_close_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Cm, Channel} = ?config(sftp, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    ReqId = 0,

    {ok, <<?SSH_FXP_HANDLE, ?UINT32(ReqId), Handle/binary>>, _} =
	open_dir(PrivDir, Cm, Channel, ReqId),

    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	  ?UINT32(?SSH_FX_OK), _/binary>>, _} = close(Handle, ReqId,
						      Cm, Channel),

    NewReqId = 1,
    case open_dir(FileName, Cm, Channel, NewReqId) of
	{ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId),
	      ?UINT32(?SSH_FX_NOT_A_DIRECTORY), _/binary>>, _} ->
	    %% Only if server is using vsn > 5.
	    ok;
	{ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId),
	      ?UINT32(?SSH_FX_FAILURE), _/binary>>, _} ->
	    ok
    end.

%%--------------------------------------------------------------------
read_file(doc) ->
    ["Test SSH_FXP_READ command"];
read_file(suite) ->
    [];
read_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),

    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, <<?SSH_FXP_HANDLE, ?UINT32(ReqId), Handle/binary>>, _} =
	open_file(FileName, Cm, Channel, ReqId,
		  ?ACE4_READ_DATA  bor ?ACE4_READ_ATTRIBUTES,
		  ?SSH_FXF_OPEN_EXISTING),

    NewReqId = 1,

    {ok, <<?SSH_FXP_DATA, ?UINT32(NewReqId), ?UINT32(_Length),
	  Data/binary>>, _} =
	read_file(Handle, 100, 0, Cm, Channel, NewReqId),

    {ok, Data} = file:read_file(FileName),

    ok.
%%--------------------------------------------------------------------
read_dir(doc) ->
    ["Test SSH_FXP_READDIR command"];
read_dir(suite) ->
    [];
read_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Cm, Channel} = ?config(sftp, Config),
    ReqId = 0,
    {ok, <<?SSH_FXP_HANDLE, ?UINT32(ReqId), Handle/binary>>, _} =
	open_dir(PrivDir, Cm, Channel, ReqId),
    ok = read_dir(Handle, Cm, Channel, ReqId),
    ok.

%%--------------------------------------------------------------------
write_file(doc) ->
    ["Test SSH_FXP_WRITE command"];
write_file(suite) ->
    [];
write_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),

    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, <<?SSH_FXP_HANDLE, ?UINT32(ReqId), Handle/binary>>, _} =
	open_file(FileName, Cm, Channel, ReqId,
		  ?ACE4_WRITE_DATA  bor ?ACE4_WRITE_ATTRIBUTES,
		  ?SSH_FXF_OPEN_EXISTING),

    NewReqId = 1,
    Data =  list_to_binary("Write file test"),

    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId), ?UINT32(?SSH_FX_OK),
	  _/binary>>, _}
	= write_file(Handle, Data, 0, Cm, Channel, NewReqId),

    {ok, Data} = file:read_file(FileName),

    ok.

%%--------------------------------------------------------------------
remove_file(doc) ->
    ["Test SSH_FXP_REMOVE command"];
remove_file(suite) ->
    [];
remove_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	   ?UINT32(?SSH_FX_OK), _/binary>>, _} =
	remove(FileName, Cm, Channel, ReqId),

    NewReqId = 1,
    %%  {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId), % ver 6 we have 5
    %% 	  ?UINT32(?SSH_FX_FILE_IS_A_DIRECTORY ), _/binary>>, _} =

    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId),
	  ?UINT32(?SSH_FX_FAILURE), _/binary>>, _} =
	remove(PrivDir, Cm, Channel, NewReqId),

    ok.

%%--------------------------------------------------------------------
rename_file(doc) ->
    ["Test SSH_FXP_RENAME command"];
rename_file(suite) ->
    [];
rename_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    NewFileName = filename:join(PrivDir, "test1.txt"),
    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	  ?UINT32(?SSH_FX_OK), _/binary>>, _} =
	rename(FileName, NewFileName, Cm, Channel, ReqId, 6, 0),

    NewReqId = ReqId + 1,

    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId),
	  ?UINT32(?SSH_FX_OK), _/binary>>, _} =
	rename(NewFileName, FileName, Cm, Channel, NewReqId, 6,
	       ?SSH_FXP_RENAME_OVERWRITE),

    NewReqId1 = NewReqId + 1,
    file:copy(FileName, NewFileName),

    %% No owerwrite
    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId1),
	  ?UINT32(?SSH_FX_FILE_ALREADY_EXISTS), _/binary>>, _} =
	rename(FileName, NewFileName, Cm, Channel, NewReqId1, 6,
	       ?SSH_FXP_RENAME_NATIVE),

    NewReqId2 = NewReqId1 + 1,

    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId2),
	  ?UINT32(?SSH_FX_OP_UNSUPPORTED), _/binary>>, _} =
	rename(FileName, NewFileName, Cm, Channel, NewReqId2, 6,
	       ?SSH_FXP_RENAME_ATOMIC),

    ok.

%%--------------------------------------------------------------------
mk_rm_dir(doc) ->
    ["Test SSH_FXP_MKDIR and SSH_FXP_RMDIR command"];
mk_rm_dir(suite) ->
    [];
mk_rm_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Cm, Channel} = ?config(sftp, Config),
    DirName = filename:join(PrivDir, "test"),
    ReqId = 0,
    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId), ?UINT32(?SSH_FX_OK),
	  _/binary>>, _} = mkdir(DirName, Cm, Channel, ReqId),

    NewReqId = 1,
    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId), ?UINT32(?SSH_FX_FAILURE),
	  _/binary>>, _} = mkdir(DirName, Cm, Channel, NewReqId),

    NewReqId1 = 2,
    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId1), ?UINT32(?SSH_FX_OK),
	    _/binary>>, _} = rmdir(DirName, Cm, Channel, NewReqId1),

    NewReqId2 = 3,
    {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId2), ?UINT32(?SSH_FX_NO_SUCH_FILE),
	    _/binary>>, _} = rmdir(DirName, Cm, Channel, NewReqId2),

    ok.
%%--------------------------------------------------------------------
real_path(doc) ->
    ["Test SSH_FXP_REALPATH command"];
real_path(suite) ->
    [];
real_path(Config) when is_list(Config) ->
    case test_server:os_type() of
	{win32, _} ->
	    {skip,  "Not a relevant test on windows"};
	_ ->
	    ReqId = 0,
	    {Cm, Channel} = ?config(sftp, Config),
	    PrivDir = ?config(priv_dir, Config),
	    TestDir = filename:join(PrivDir, "ssh_test"),
	    ok = file:make_dir(TestDir),

	    OrigPath = filename:join(TestDir, ".."),

	    {ok, <<?SSH_FXP_NAME, ?UINT32(ReqId), ?UINT32(_), ?UINT32(Len),
	     Path:Len/binary, _/binary>>, _}
		= real_path(OrigPath, Cm, Channel, ReqId),

	    RealPath = filename:absname(binary_to_list(Path)),
	    AbsPrivDir = filename:absname(PrivDir),

	    test_server:format("Path: ~p PrivDir: ~p~n", [RealPath, AbsPrivDir]),

	    true = RealPath == AbsPrivDir,

	    ok
    end.

%%--------------------------------------------------------------------
links(doc) ->
    [];
links(suite) ->
    [];
links(Config) when is_list(Config) ->
    case test_server:os_type() of
	{win32, _} ->
	    {skip, "Links are not fully supported by windows"};
	_ ->
	    ReqId = 0,
	    {Cm, Channel} = ?config(sftp, Config),
	    PrivDir =  ?config(priv_dir, Config),
	    FileName = filename:join(PrivDir, "test.txt"),
	    LinkFileName = filename:join(PrivDir, "link_test.txt"),

	    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
		  ?UINT32(?SSH_FX_OK), _/binary>>, _} =
		create_link(LinkFileName, FileName, Cm, Channel, ReqId),

	    NewReqId = 1,
	    {ok, <<?SSH_FXP_NAME, ?UINT32(NewReqId), ?UINT32(_), ?UINT32(Len),
		  Path:Len/binary, _/binary>>, _}
		= read_link(LinkFileName, Cm, Channel, NewReqId),


	    true = binary_to_list(Path) == FileName,

	    test_server:format("Path: ~p~n", [binary_to_list(Path)]),
	    ok
    end.

%%--------------------------------------------------------------------
retrieve_attributes(doc) ->
    ["Test SSH_FXP_STAT, SSH_FXP_LSTAT AND SSH_FXP_FSTAT commands"];
retrieve_attributes(suite) ->
    [];
retrieve_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, FileInfo} = file:read_file_info(FileName),

    AttrValues =
	retrive_attributes(FileName, Cm, Channel, ReqId),

    Type =  encode_file_type(FileInfo#file_info.type),
    Size = FileInfo#file_info.size,
    Owner = FileInfo#file_info.uid,
    Group =  FileInfo#file_info.gid,
    Permissions = FileInfo#file_info.mode,
    Atime =  calendar:datetime_to_gregorian_seconds(
	       erlang:localtime_to_universaltime(FileInfo#file_info.atime))
	-  ?UNIX_EPOCH,
    Mtime =  calendar:datetime_to_gregorian_seconds(
	       erlang:localtime_to_universaltime(FileInfo#file_info.mtime))
	-  ?UNIX_EPOCH,
    Ctime =  calendar:datetime_to_gregorian_seconds(
	       erlang:localtime_to_universaltime(FileInfo#file_info.ctime))
	-  ?UNIX_EPOCH,

    lists:foreach(fun(Value) ->
			  <<?UINT32(Flags), _/binary>> = Value,
			  true = ?is_set(?SSH_FILEXFER_ATTR_SIZE,
					Flags),
			  true = ?is_set(?SSH_FILEXFER_ATTR_PERMISSIONS,
					Flags),
			  true = ?is_set(?SSH_FILEXFER_ATTR_ACCESSTIME,
					Flags),
			  true = ?is_set(?SSH_FILEXFER_ATTR_CREATETIME,
					Flags),
			  true = ?is_set(?SSH_FILEXFER_ATTR_MODIFYTIME,
					Flags),
			  true = ?is_set(?SSH_FILEXFER_ATTR_OWNERGROUP,
					 Flags),
			  false = ?is_set(?SSH_FILEXFER_ATTR_ACL,
					Flags),
			  false = ?is_set(?SSH_FILEXFER_ATTR_SUBSECOND_TIMES,
					Flags),
			  false = ?is_set(?SSH_FILEXFER_ATTR_BITS,
					Flags),
			  false = ?is_set(?SSH_FILEXFER_ATTR_EXTENDED,
					Flags),

			  <<?UINT32(_Flags), ?BYTE(Type),
			   ?UINT64(Size),
			   ?UINT32(OwnerLen), BinOwner:OwnerLen/binary,
			   ?UINT32(GroupLen), BinGroup:GroupLen/binary,
			   ?UINT32(Permissions),
			   ?UINT64(Atime),
			   ?UINT64(Ctime),
			   ?UINT64(Mtime)>> = Value,

			  Owner = list_to_integer(binary_to_list(BinOwner)),
			  Group =  list_to_integer(binary_to_list(BinGroup))
		  end, AttrValues),

    ok.
%%--------------------------------------------------------------------
set_attributes(doc) ->
    ["Test SSH_FXP_SETSTAT AND SSH_FXP_FSETSTAT commands"];
set_attributes(suite) ->
    [];
set_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, FileInfo} = file:read_file_info(FileName),

    OrigPermissions = FileInfo#file_info.mode,
    Permissions = 8#400, %% User read-only

    Flags = ?SSH_FILEXFER_ATTR_PERMISSIONS,

    Atters = [?uint32(Flags), ?byte(?SSH_FILEXFER_TYPE_REGULAR),
	      ?uint32(Permissions)],

    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	  ?UINT32(?SSH_FX_OK), _/binary>>, _} =
	set_attributes_file(FileName, Atters, Cm, Channel, ReqId),

    {ok, NewFileInfo} = file:read_file_info(FileName),
    NewPermissions = NewFileInfo#file_info.mode,

    %% Can not test that NewPermissions = Permissions as
    %% on Unix platforms, other bits than those listed in the
    %% API may be set.
    test_server:format("Org: ~p New: ~p~n", [OrigPermissions, NewPermissions]),
    true = OrigPermissions =/= NewPermissions,

    test_server:format("Try to open the file"),
    NewReqId = 2,
    {ok, <<?SSH_FXP_HANDLE, ?UINT32(NewReqId), Handle/binary>>, _} =
	open_file(FileName, Cm, Channel, NewReqId,
		  ?ACE4_READ_DATA bor ?ACE4_WRITE_ATTRIBUTES,
		  ?SSH_FXF_OPEN_EXISTING),

    NewAtters = [?uint32(Flags), ?byte(?SSH_FILEXFER_TYPE_REGULAR),
		 ?uint32(OrigPermissions)],

    NewReqId1 = 3,

    test_server:format("Set original permissions on the now open file"),

     {ok, <<?SSH_FXP_STATUS, ?UINT32(NewReqId1),
	  ?UINT32(?SSH_FX_OK), _/binary>>, _} =
	set_attributes_open_file(Handle, NewAtters, Cm, Channel, NewReqId1),

    {ok, NewFileInfo1} = file:read_file_info(FileName),
    OrigPermissions = NewFileInfo1#file_info.mode,

    ok.

%%--------------------------------------------------------------------
ver3_rename_OTP_6352(doc) ->
    ["Test that ver3 rename message is handled"];

ver3_rename_OTP_6352(suite) ->
    [];

ver3_rename_OTP_6352(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    NewFileName = filename:join(PrivDir, "test1.txt"),
    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	  ?UINT32(?SSH_FX_OK), _/binary>>, _} =
	rename(FileName, NewFileName, Cm, Channel, ReqId, 3, 0),

    ok.

%%--------------------------------------------------------------------
seq10670(doc) ->
    ["Check that realpath works ok"];

seq10670(suite) ->
    [];

seq10670(Config) when is_list(Config) ->
    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    case test_server:os_type() of
	{win32, _} ->
	    {skip,  "Not a relevant test on windows"};
	_ ->
	    {ok, <<?SSH_FXP_NAME, ?UINT32(ReqId), ?UINT32(_), ?UINT32(Len),
		  Root:Len/binary, _/binary>>, _}
		= real_path("/..", Cm, Channel, ReqId),

	    <<"/">> = Root,

	    {ok, <<?SSH_FXP_NAME, ?UINT32(ReqId), ?UINT32(_), ?UINT32(Len),
		  Path:Len/binary, _/binary>>, _}
		= real_path("/usr/bin/../..", Cm, Channel, ReqId),

	    Root = Path
    end.

%% Internal functions
%%--------------------------------------------------------------------
prep(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    TestFile = filename:join(PrivDir, "test.txt"),
    TestFile1 = filename:join(PrivDir, "test1.txt"),

    file:delete(TestFile),
    file:delete(TestFile1),

    %% Initial config
    DataDir = ?config(data_dir, Config),
    FileName = filename:join(DataDir, "test.txt"),
    file:copy(FileName, TestFile),
    Mode = 8#00400 bor 8#00200 bor 8#00040, % read & write owner, read group
    {ok, FileInfo} = file:read_file_info(TestFile),
    ok = file:write_file_info(TestFile,
			      FileInfo#file_info{mode = Mode}).

reply(Cm, Channel) ->
    reply(Cm, Channel,<<>>).

reply(Cm, Channel, RBuf) ->
    receive
	{ssh_cm, Cm, {data, Channel, 0, Data}} ->
	    case <<RBuf/binary, Data/binary>> of
		<<?UINT32(Len),Reply:Len/binary,Rest/binary>> ->
		    {ok, Reply, Rest};
		RBuf2 ->
		    reply(Cm, Channel, RBuf2)
	    end;
	{ssh_cm, Cm, {eof, Channel}} ->
	    eof;
	{ssh_cm, Cm, {closed, Channel}} ->
	    closed;
	{ssh_cm, Cm, Msg} ->
	    test_server:fail(Msg)
    end.


open_file(File, Cm, Channel, ReqId, Access, Flags) ->

    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(File)),
			   ?uint32(Access),
			   ?uint32(Flags),
			   ?REG_ATTERS]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
				      ?SSH_FXP_OPEN, Data/binary>>),
    reply(Cm, Channel).



close(Handle, ReqId, Cm , Channel) ->
    Data = list_to_binary([?uint32(ReqId), Handle]),

    Size = 1 + size(Data),

    ssh_connection:send(Cm, Channel, <<?UINT32(Size), ?SSH_FXP_CLOSE,
			      Data/binary>>),

    reply(Cm, Channel).



open_dir(Dir, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(Dir))]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
				      ?SSH_FXP_OPENDIR, Data/binary>>),
    reply(Cm, Channel).


rename(OldName, NewName, Cm, Channel, ReqId, Version, Flags) ->
    Data =
	case Version of
	    3 ->
		list_to_binary([?uint32(ReqId),
				?binary(list_to_binary(OldName)),
				?binary(list_to_binary(NewName))]);
	    _ ->
		list_to_binary([?uint32(ReqId),
				?binary(list_to_binary(OldName)),
				?binary(list_to_binary(NewName)),
				?uint32(Flags)])
	end,
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
				      ?SSH_FXP_RENAME, Data/binary>>),
    reply(Cm, Channel).


mkdir(Dir, Cm, Channel, ReqId)->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(Dir)),
			   ?REG_ATTERS]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
				      ?SSH_FXP_MKDIR, Data/binary>>),
    reply(Cm, Channel).


rmdir(Dir, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(Dir))]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_RMDIR, Data/binary>>),
    reply(Cm, Channel).

remove(File, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(File))]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_REMOVE, Data/binary>>),
    reply(Cm, Channel).


read_dir(Handle, Cm, Channel, ReqId) ->

    Data = list_to_binary([?uint32(ReqId), Handle]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_READDIR, Data/binary>>),
    case reply(Cm, Channel) of
	{ok, <<?SSH_FXP_NAME, ?UINT32(ReqId), ?UINT32(Count),
	       ?UINT32(Len), Listing:Len/binary, _/binary>>, _} ->
	    test_server:format("Count: ~p Listing: ~p~n",
			       [Count, binary_to_list(Listing)]),
	    read_dir(Handle, Cm, Channel, ReqId);
	{ok, <<?SSH_FXP_STATUS, ?UINT32(ReqId),
	      ?UINT32(?SSH_FX_EOF), _/binary>>, _}  ->
	    ok
    end.

read_file(Handle, MaxLength, OffSet, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId), Handle,
			   ?uint64(OffSet),
			   ?uint32(MaxLength)]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_READ, Data/binary>>),
    reply(Cm, Channel).


write_file(Handle, FileData, OffSet, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId), Handle,
			   ?uint64(OffSet),
			   ?binary(FileData)]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_WRITE, Data/binary>>),
    reply(Cm, Channel).


real_path(OrigPath, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(OrigPath))]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_REALPATH, Data/binary>>),
    reply(Cm, Channel).

create_link(LinkPath, Path, Cm, Channel, ReqId) ->
     Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(LinkPath)),
			   ?binary(list_to_binary(Path))]),
     Size = 1 + size(Data),
     ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_SYMLINK, Data/binary>>),
     reply(Cm, Channel).


read_link(Link, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(Link))]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_READLINK, Data/binary>>),
    reply(Cm, Channel).

retrive_attributes_file(FilePath, Flags, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(FilePath)),
			   ?uint32(Flags)]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_STAT, Data/binary>>),
    reply(Cm, Channel).

retrive_attributes_file_or_link(FilePath, Flags, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(FilePath)),
			   ?uint32(Flags)]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_LSTAT, Data/binary>>),
    reply(Cm, Channel).

retrive_attributes_open_file(Handle, Flags, Cm, Channel, ReqId) ->

    Data = list_to_binary([?uint32(ReqId),
			   Handle,
			   ?uint32(Flags)]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_FSTAT, Data/binary>>),
    reply(Cm, Channel).

retrive_attributes(FileName, Cm, Channel, ReqId) ->

    Attr =  ?SSH_FILEXFER_ATTR_SIZE,

    {ok, <<?SSH_FXP_ATTRS, ?UINT32(ReqId), Value/binary>>, _}
	= retrive_attributes_file(FileName, Attr,
				  Cm, Channel, ReqId),

    NewReqId = ReqId + 1,
    {ok, <<?SSH_FXP_ATTRS, ?UINT32(NewReqId), Value1/binary>>, _}
	= retrive_attributes_file_or_link(FileName,
					  Attr, Cm, Channel, NewReqId),

    NewReqId1 = NewReqId + 1,
    {ok, <<?SSH_FXP_HANDLE, ?UINT32(NewReqId1), Handle/binary>>, _} =
	open_file(FileName, Cm, Channel, NewReqId1,
		  ?ACE4_READ_DATA  bor ?ACE4_READ_ATTRIBUTES,
		  ?SSH_FXF_OPEN_EXISTING),

    NewReqId2 = NewReqId1 + 1,
    {ok, <<?SSH_FXP_ATTRS, ?UINT32(NewReqId2), Value2/binary>>, _}
	= retrive_attributes_open_file(Handle, Attr, Cm, Channel, NewReqId2),

    [Value, Value1, Value2].

set_attributes_file(FilePath, Atters, Cm, Channel, ReqId) ->
    Data = list_to_binary([?uint32(ReqId),
			   ?binary(list_to_binary(FilePath)),
			   Atters]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_SETSTAT, Data/binary>>),
    reply(Cm, Channel).


set_attributes_open_file(Handle, Atters, Cm, Channel, ReqId) ->

    Data = list_to_binary([?uint32(ReqId),
			   Handle,
			   Atters]),
    Size = 1 + size(Data),
    ssh_connection:send(Cm, Channel, <<?UINT32(Size),
			      ?SSH_FXP_FSETSTAT, Data/binary>>),
    reply(Cm, Channel).


encode_file_type(Type) ->
    case Type of
	regular -> ?SSH_FILEXFER_TYPE_REGULAR;
	directory -> ?SSH_FILEXFER_TYPE_DIRECTORY;
	symlink -> ?SSH_FILEXFER_TYPE_SYMLINK;
	special -> ?SSH_FILEXFER_TYPE_SPECIAL;
	unknown -> ?SSH_FILEXFER_TYPE_UNKNOWN;
	other -> ?SSH_FILEXFER_TYPE_UNKNOWN;
	socket -> ?SSH_FILEXFER_TYPE_SOCKET;
	char_device -> ?SSH_FILEXFER_TYPE_CHAR_DEVICE;
	block_device -> ?SSH_FILEXFER_TYPE_BLOCK_DEVICE;
	fifo -> ?SSH_FILEXFER_TYPE_FIFO;
	undefined -> ?SSH_FILEXFER_TYPE_UNKNOWN
    end.

%%--------------------------------------------------------------------
sshd_read_file(doc) ->
    ["Test SSH_FXP_READ command, using sshd-server"];
sshd_read_file(suite) ->
    [];
sshd_read_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),

    ReqId = 0,
    {Cm, Channel} = ?config(sftp, Config),

    {ok, <<?SSH_FXP_HANDLE, ?UINT32(ReqId), Handle/binary>>, _} =
	open_file(FileName, Cm, Channel, ReqId,
		  ?ACE4_READ_DATA  bor ?ACE4_READ_ATTRIBUTES,
		  ?SSH_FXF_OPEN_EXISTING),

    NewReqId = 1,

    {ok, <<?SSH_FXP_DATA, ?UINT32(NewReqId), ?UINT32(_Length),
	  Data/binary>>, _} =
	read_file(Handle, 100, 0, Cm, Channel, NewReqId),

    {ok, Data} = file:read_file(FileName),

    ok.
