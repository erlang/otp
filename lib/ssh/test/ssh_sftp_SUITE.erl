%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2014. All Rights Reserved.
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
-module(ssh_sftp_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

% Default timetrap timeout
-define(default_timeout, ?t:minutes(1)).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, not_unicode},
     {group, unicode}
    ].


init_per_suite(Config) ->
    catch crypto:stop(),
    case (catch crypto:start()) of
	ok ->
	    ct:pal("file:native_name_encoding() = ~p,~nio:getopts() = ~p",
		   [file:native_name_encoding(),io:getopts()]),
	    ssh:start(),
	    Config;
	_ ->
	    {skip,"Could not start crypto!"}
    end.

end_per_suite(Config) ->
    ssh:stop(),
    crypto:stop(),
    Config.

%%--------------------------------------------------------------------
groups() -> 
    [{not_unicode, [], [{group,erlang_server},
			{group,openssh_server},
			sftp_nonexistent_subsystem]},

     {unicode, [], [{group,erlang_server},
		    {group,openssh_server},
		    sftp_nonexistent_subsystem]},
     
     {erlang_server, [], [{group,write_read_tests},
			  version_option,
			  {group,remote_tar}]},

     {openssh_server, [], [{group,write_read_tests},
			   {group,remote_tar}]},

     {remote_tar, [], [create_empty_tar, files_to_tar, big_file_to_tar, files_chunked_to_tar,
		       directory_to_tar, binaries_to_tar, null_crypto_tar, 
		       simple_crypto_tar_small, simple_crypto_tar_big,
		       read_tar, read_null_crypto_tar, read_crypto_tar, 
		       aes_cbc256_crypto_tar, aes_ctr_stream_crypto_tar
		      ]},

     {write_read_tests, [], [open_close_file, open_close_dir, read_file, read_dir,
			     write_file, write_file_iolist, write_big_file, sftp_read_big_file,
			     rename_file, mk_rm_dir, remove_file, links,
			     retrieve_attributes, set_attributes, async_read,
			     async_write, position, pos_read, pos_write
			    ]}
    ].

init_per_group(not_unicode, Config) ->
    ct:comment("Begin ~p",[grps(Config)]),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    [{user, "Alladin"},
     {passwd, "Sesame"},
     {data, <<"Hello world!">>},
     {filename, filename:join(PrivDir, "sftp.txt")},
     {testfile, filename:join(PrivDir, "test.txt")},
     {linktest, filename:join(PrivDir, "link_test.txt")},
     {tar_filename, filename:join(PrivDir,  "sftp_tar_test.tar")},
     {tar_F1_txt, "f1.txt"},
     {datadir_tar, filename:join(DataDir,"sftp_tar_test_data")}
     | Config];

init_per_group(unicode, Config) ->
    case file:native_name_encoding() of
	utf8 ->
	    ct:comment("Begin ~p",[grps(Config)]),
	    DataDir = ?config(data_dir, Config),
	    PrivDir = ?config(priv_dir, Config),
	    [{user, "åke高兴"},
	     {passwd, "ärlig日本じん"},
	     {data, <<"foobar å 一二三四いちにさんち">>},
	     {filename, filename:join(PrivDir, "sftp瑞点.txt")},
	     {testfile, filename:join(PrivDir, "testハンス.txt")},
	     {linktest, filename:join(PrivDir, "link_test語.txt")},
	     {tar_filename, filename:join(PrivDir,  "sftp_tar_test一二三.tar")},
	     {tar_F1_txt, "F一.txt"},
	     {datadir_tar, filename:join(DataDir,"sftp_tar_test_data_高兴")}
	     | lists:foldl(fun(K,Cf) -> lists:keydelete(K,1,Cf) end, 
			   Config,
			   [user, passwd, data,
			    filename, testfile, linktest,
			    tar_filename, tar_F1_txt, datadir_tar
			   ]
			  )
	    ];

	_ ->
	    {skip, "Not unicode file encoding"}
    end;
		
init_per_group(erlang_server, Config) ->
    ct:comment("Begin ~p",[grps(Config)]),
    PrivDir = ?config(priv_dir, Config),
    SysDir =  ?config(data_dir, Config),
    User = ?config(user, Config),
    Passwd = ?config(passwd, Config),
    Sftpd = {_, HostX, PortX} =
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, PrivDir},
			     {user_passwords,
			      [{User, Passwd}]}]),
    [{peer, {fmt_host(HostX),PortX}}, {group, erlang_server}, {sftpd, Sftpd} | Config];

init_per_group(openssh_server, Config) ->
    ct:comment("Begin ~p",[grps(Config)]),
    Host = ssh_test_lib:hostname(),
    case (catch ssh_sftp:start_channel(Host,					      
				       [{user_interaction, false},
					{silently_accept_hosts, true}])) of
	{ok, _ChannelPid, Connection} ->
	    [{peer, {_HostName,{IPx,Portx}}}] = ssh:connection_info(Connection,[peer]),
	    ssh:close(Connection),
	    [{peer, {fmt_host(IPx),Portx}}, {group, openssh_server} | Config];
	_ ->
	    {skip, "No openssh server"} 
    end;

init_per_group(remote_tar, Config) ->
    ct:comment("Begin ~p",[grps(Config)]),
    {Host,Port} =  ?config(peer, Config),
    ct:log("Server (~p) at ~p:~p",[?config(group,Config),Host,Port]),
    User = ?config(user, Config),
    Passwd = ?config(passwd, Config),
    {ok, Connection} =
	case ?config(group, Config) of
	    erlang_server ->
		ssh:connect(Host, Port,
			    [{user, User},
			     {password, Passwd},
			     {user_interaction, false},
			     {silently_accept_hosts, true}]);
	    openssh_server ->
		ssh:connect(Host, Port,
			    [{user_interaction, false},
			     {silently_accept_hosts, true}])
	end,
    [{remote_tar, true}, 
     {connection, Connection} | Config];

init_per_group(write_read_tests, Config) ->
    ct:comment("Begin ~p",[grps(Config)]),
    Config.

grps(Config) ->
    proplists:get_all_values(
      name, 
      lists:flatten([proplists:get_value(tc_group_properties,Config,[]),
		     proplists:get_value(tc_group_path,Config,[])])).


end_per_group(erlang_server, Config) ->
    ct:comment("End ~p",[grps(Config)]),
    Config;
end_per_group(_, Config) ->
    ct:comment("End ~p",[grps(Config)]),
    Config.

%%--------------------------------------------------------------------

init_per_testcase(sftp_nonexistent_subsystem, Config) ->
    PrivDir = ?config(priv_dir, Config),
    SysDir =  ?config(data_dir, Config),
    User = ?config(user, Config),
    Passwd = ?config(passwd, Config),
    Sftpd = ssh_test_lib:daemon([{system_dir, SysDir},
				 {user_dir, PrivDir},
				 {subsystems, []},
				 {user_passwords,
				  [{User, Passwd}]}
				]),
    [{sftpd, Sftpd} | Config];

init_per_testcase(version_option, Config) ->
    prep(Config),
    TmpConfig0 = lists:keydelete(watchdog, 1, Config),
    TmpConfig = lists:keydelete(sftp, 1, TmpConfig0),
    Dog = ct:timetrap(?default_timeout),
    {_,Host, Port} =  ?config(sftpd, Config),
    User = ?config(user, Config),
    Passwd = ?config(passwd, Config),
    {ok, ChannelPid, Connection}  = 
	ssh_sftp:start_channel(Host, Port,
			       [{sftp_vsn, 3},
				{user, User},
				{password, Passwd},
				{user_interaction, false},
				{silently_accept_hosts, true}]),
    Sftp = {ChannelPid, Connection},
    [{sftp,Sftp}, {watchdog, Dog} | TmpConfig];

init_per_testcase(Case, Config0) ->
    prep(Config0),
    Config1 = lists:keydelete(watchdog, 1, Config0),
    Config2 = lists:keydelete(sftp, 1, Config1),
    Dog = ct:timetrap(?default_timeout),
    User = ?config(user, Config0),
    Passwd = ?config(passwd, Config0),

    Config =
	case ?config(group,Config2) of
	    erlang_server ->
		{_,Host, Port} =  ?config(sftpd, Config2),
		{ok, ChannelPid, Connection}  = 
		    ssh_sftp:start_channel(Host, Port,
					   [{user, User},
					    {password, Passwd},
					    {user_interaction, false},
					    {silently_accept_hosts, true}]),
		Sftp = {ChannelPid, Connection},
		[{sftp, Sftp}, {watchdog, Dog} | Config2];
	    openssh_server when Case == links ->
		{skip, "known bug in openssh"};
	    openssh_server ->
		Host = ssh_test_lib:hostname(),
		{ok, ChannelPid, Connection} = 
		    ssh_sftp:start_channel(Host, 
					   [{user_interaction, false},
					    {silently_accept_hosts, true}]), 
		Sftp = {ChannelPid, Connection},
		[{sftp, Sftp}, {watchdog, Dog} | Config2]
	end,

    case catch ?config(remote_tar,Config) of
	%% The 'catch' is for the case of Config={skip,...}
	true ->
	    %% Provide a ChannelPid independent of the sftp-channel already opened.
	    {ok,ChPid2} = ssh_sftp:start_channel(?config(connection,Config)),
	    [{channel_pid2,ChPid2} | Config];
	_ ->
	    Config
    end.

end_per_testcase(sftp_nonexistent_subsystem, Config) ->
    Config;
end_per_testcase(rename_file, Config) ->
    NewFileName = ?config(testfile, Config),
    file:delete(NewFileName),  
    end_per_testcase(Config);
end_per_testcase(_, Config) ->
    end_per_testcase(Config).

end_per_testcase(Config) ->
    {Sftp, Connection} = ?config(sftp, Config),
    ssh_sftp:stop_channel(Sftp),
    catch ssh_sftp:stop_channel(?config(channel_pid2, Config)),
    ssh:close(Connection).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
open_close_file() ->
    [{doc, "Test API functions open/3 and close/2"}].
open_close_file(Config) when is_list(Config) ->
    FileName = ?config(filename, Config),

    {Sftp, _} = ?config(sftp, Config),

    ok = open_close_file(Sftp, FileName, [read]),
    ok = open_close_file(Sftp, FileName, [write]),
    ok = open_close_file(Sftp, FileName, [write, creat]),
    ok = open_close_file(Sftp, FileName, [write, trunc]),
    ok = open_close_file(Sftp, FileName, [append]),
    ok = open_close_file(Sftp, FileName, [read, binary]).

open_close_file(Server, File, Mode) ->
    {ok, Handle} = ssh_sftp:open(Server, File, Mode),
    ok = ssh_sftp:close(Server, Handle).

%%--------------------------------------------------------------------
open_close_dir() ->
    [{doc, "Test API functions opendir/2 and close/2"}].
open_close_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
    FileName = ?config(filename, Config),

    {ok, Handle} = ssh_sftp:opendir(Sftp, PrivDir),
    ok = ssh_sftp:close(Sftp, Handle),
    {error, _} =  ssh_sftp:opendir(Sftp, FileName).

%%--------------------------------------------------------------------
read_file() ->
    [{doc, "Test API funtion read_file/2"}].
read_file(Config) when is_list(Config) ->
    FileName = ?config(filename, Config),
    {Sftp, _} = ?config(sftp, Config),
    {ok, Data} = ssh_sftp:read_file(Sftp, FileName),
    {ok, Data} = ssh_sftp:read_file(Sftp, FileName),
    {ok, Data} = file:read_file(FileName).

%%--------------------------------------------------------------------
read_dir() ->
    [{doc,"Test API function list_dir/2"}].
read_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),
    ct:pal("sftp list dir: ~p~n", [Files]).

%%--------------------------------------------------------------------
write_file() ->
    [{doc, "Test API function write_file/2"}].
write_file(Config) when is_list(Config) ->
    FileName = ?config(filename, Config),
    {Sftp, _} = ?config(sftp, Config),

    Data = list_to_binary("Hej hopp!"),
    ssh_sftp:write_file(Sftp, FileName, [Data]),
    {ok, Data} = file:read_file(FileName).

%%--------------------------------------------------------------------
write_file_iolist() ->
    [{doc, "Test API function write_file/2 with iolists"}].
write_file_iolist(Config) when is_list(Config) ->
    FileName = ?config(filename, Config),
    {Sftp, _} = ?config(sftp, Config),

    Data = list_to_binary("Hej hopp!"),
    lists:foreach(
      fun(D) ->
	      ssh_sftp:write_file(Sftp, FileName, [D]),
	      Expected = if is_binary(D) -> D;
			    is_list(D) -> list_to_binary(D)
			 end,
	      {ok, Expected} = file:read_file(FileName)
      end,
      [Data, [Data,Data], [[Data],[Data]], [[[Data]],[[[[Data]],Data]]],
       [[[[Data]],Data],binary_to_list(Data)],
       [[[[Data]],Data],[[binary_to_list(Data)],[[binary_to_list(Data)]]]]
      ]).

%%--------------------------------------------------------------------
write_big_file() ->
    [{doc, "Test API function write_file/2 with big data"}].
write_big_file(Config) when is_list(Config) ->
    FileName = ?config(filename, Config),
    {Sftp, _} = ?config(sftp, Config),

    Data = list_to_binary(lists:duplicate(750000,"a")),
    ssh_sftp:write_file(Sftp, FileName, [Data]),
    {ok, Data} = file:read_file(FileName).

%%--------------------------------------------------------------------
sftp_read_big_file() ->
    [{doc, "Test API function read_file/2 with big data"}].
sftp_read_big_file(Config) when is_list(Config) ->
    FileName = ?config(filename, Config),
    {Sftp, _} = ?config(sftp, Config),

    Data = list_to_binary(lists:duplicate(750000,"a")),
    ct:log("Data size to write is ~p bytes",[size(Data)]),
    ssh_sftp:write_file(Sftp, FileName, [Data]),
    {ok, Data} = ssh_sftp:read_file(Sftp, FileName).

%%--------------------------------------------------------------------
remove_file() ->
    [{doc,"Test API function delete/2"}].
remove_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = ?config(filename, Config),
    {Sftp, _} = ?config(sftp, Config),

    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),
    true = lists:member(filename:basename(FileName), Files),
    ok = ssh_sftp:delete(Sftp, FileName),
    {ok, NewFiles} = ssh_sftp:list_dir(Sftp, PrivDir),
    false = lists:member(filename:basename(FileName), NewFiles),
    {error, _} = ssh_sftp:delete(Sftp, FileName).
%%--------------------------------------------------------------------
rename_file() ->
    [{doc, "Test API function rename_file/2"}].
rename_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = ?config(filename, Config),
    NewFileName = ?config(testfile, Config),

    {Sftp, _} = ?config(sftp, Config),
    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),
    ct:pal("FileName: ~p, Files: ~p~n", [FileName, Files]),
    true = lists:member(filename:basename(FileName), Files),
    false = lists:member(filename:basename(NewFileName), Files),
    ok = ssh_sftp:rename(Sftp, FileName, NewFileName),
    {ok, NewFiles} = ssh_sftp:list_dir(Sftp, PrivDir),
    ct:pal("FileName: ~p, Files: ~p~n", [FileName, NewFiles]),

    false = lists:member(filename:basename(FileName), NewFiles),
    true = lists:member(filename:basename(NewFileName), NewFiles).

%%--------------------------------------------------------------------
mk_rm_dir() ->
    [{doc,"Test API functions make_dir/2, del_dir/2"}].
mk_rm_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
 
    DirName = filename:join(PrivDir, "test"),
    ok = ssh_sftp:make_dir(Sftp, DirName),
    ok = ssh_sftp:del_dir(Sftp, DirName),
    NewDirName = filename:join(PrivDir, "foo/bar"),
    {error, _} = ssh_sftp:make_dir(Sftp, NewDirName),
    {error, _} = ssh_sftp:del_dir(Sftp, PrivDir).

%%--------------------------------------------------------------------
links() ->
    [{doc,"Tests API function make_symlink/3"}].
links(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    {skip, "Links are not fully supported by windows"};
	_ ->
	    {Sftp, _} = ?config(sftp, Config),
	    FileName = ?config(filename, Config),
	    LinkFileName = ?config(linktest, Config),

	    ok = ssh_sftp:make_symlink(Sftp, LinkFileName, FileName),
	    {ok, FileName} = ssh_sftp:read_link(Sftp, LinkFileName)
    end.

%%--------------------------------------------------------------------
retrieve_attributes() ->
    [{doc, "Test API function read_file_info/3"}].
retrieve_attributes(Config) when is_list(Config) ->
    FileName = ?config(filename, Config),

    {Sftp, _} = ?config(sftp, Config),
    {ok, FileInfo} = ssh_sftp:read_file_info(Sftp, FileName),
    {ok, NewFileInfo} = file:read_file_info(FileName),

    %% TODO comparison. There are some differences now is that ok?
    ct:pal("SFTP: ~p   FILE: ~p~n", [FileInfo, NewFileInfo]).

%%--------------------------------------------------------------------
set_attributes() ->
    [{doc,"Test API function write_file_info/3"}].
set_attributes(Config) when is_list(Config) ->
    FileName = ?config(testfile, Config),

    {Sftp, _} = ?config(sftp, Config),
    {ok,Fd} = file:open(FileName, write),
    io:put_chars(Fd,"foo"),
    ok = ssh_sftp:write_file_info(Sftp, FileName, #file_info{mode=8#400}),
    {error, eacces} = file:write_file(FileName, "hello again"),
    ssh_sftp:write_file_info(Sftp, FileName, #file_info{mode=8#600}),
    ok = file:write_file(FileName, "hello again").

%%--------------------------------------------------------------------

async_read() ->
    [{doc,"Test API aread/3"}].
async_read(Config) when is_list(Config) ->
    {Sftp, _} = ?config(sftp, Config),

    FileName = ?config(filename, Config),
    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [read]),
    {async, Ref} = ssh_sftp:aread(Sftp, Handle, 20),

    receive
	{async_reply, Ref, {ok, Data}} ->
	    ct:pal("Data: ~p~n", [Data]),
	    ok;
	Msg ->
	    ct:fail(Msg)
    end.
%%--------------------------------------------------------------------
async_write() ->
    [{doc,"Test API awrite/3"}].
async_write(Config) when is_list(Config) ->
    {Sftp, _} = ?config(sftp, Config),
    FileName = ?config(testfile, Config),
    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [write]),
    Data = list_to_binary("foobar"),
    {async, Ref} = ssh_sftp:awrite(Sftp, Handle, Data),

    receive
	{async_reply, Ref, ok} ->
	    {ok, Data} = file:read_file(FileName);
	Msg ->
	    ct:fail(Msg)
    end.

%%--------------------------------------------------------------------

position() ->
    [{doc, "Test API functions position/3"}].
position(Config) when is_list(Config) ->
    FileName = ?config(testfile, Config),
    {Sftp, _} = ?config(sftp, Config),

    Data = list_to_binary("1234567890"),
    ssh_sftp:write_file(Sftp, FileName, [Data]),
    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [read]),

    {ok, 3} = ssh_sftp:position(Sftp, Handle, {bof, 3}),
    {ok, "4"} = ssh_sftp:read(Sftp, Handle, 1),

    {ok, 10} = ssh_sftp:position(Sftp, Handle, eof),
    eof = ssh_sftp:read(Sftp, Handle, 1),

    {ok, 6} = ssh_sftp:position(Sftp, Handle, {bof, 6}),
    {ok, "7"} = ssh_sftp:read(Sftp, Handle, 1),

    {ok, 9} = ssh_sftp:position(Sftp, Handle, {cur, 2}),
    {ok, "0"} = ssh_sftp:read(Sftp, Handle, 1),

    {ok, 0} = ssh_sftp:position(Sftp, Handle, bof),
    {ok, "1"} = ssh_sftp:read(Sftp, Handle, 1),

    {ok, 1} = ssh_sftp:position(Sftp, Handle, cur),
    {ok, "2"} = ssh_sftp:read(Sftp, Handle, 1).

%%--------------------------------------------------------------------
pos_read() ->
    [{doc,"Test API functions pread/3 and apread/3"}].
pos_read(Config) when is_list(Config) ->
    FileName = ?config(testfile, Config),
    {Sftp, _} = ?config(sftp, Config),
    Data = list_to_binary("Hej hopp!"),
    ssh_sftp:write_file(Sftp, FileName, [Data]),

    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [read]),
    {async, Ref} = ssh_sftp:apread(Sftp, Handle, {bof, 5}, 4),

    NewData  = "opp!",

    receive
	{async_reply, Ref, {ok, NewData}} ->
	    ok;
	Msg ->
	    ct:fail(Msg)
    end,

    NewData1  = "hopp",

    {ok, NewData1} = ssh_sftp:pread(Sftp, Handle, {bof, 4}, 4).

%%--------------------------------------------------------------------
pos_write() ->
    [{doc,"Test API functions pwrite/4 and apwrite/4"}].
pos_write(Config) when is_list(Config) ->
    FileName = ?config(testfile, Config),
    {Sftp, _} = ?config(sftp, Config),

    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [write]),

    Data = list_to_binary("Bye,"),
    ssh_sftp:write_file(Sftp, FileName, [Data]),

    NewData = list_to_binary(" see you tomorrow"),
    {async, Ref} = ssh_sftp:apwrite(Sftp, Handle, {bof, 4}, NewData),
    receive
	{async_reply, Ref, ok} ->
	    ok;
	Msg ->
	    ct:fail(Msg)
    end,

    ok = ssh_sftp:pwrite(Sftp, Handle, eof, list_to_binary("!")),

    NewData1 = list_to_binary("Bye, see you tomorrow!"),
    {ok, NewData1} = ssh_sftp:read_file(Sftp, FileName).

%%--------------------------------------------------------------------
sftp_nonexistent_subsystem() ->
    [{doc, "Try to execute sftp subsystem on a server that does not support it"}].
sftp_nonexistent_subsystem(Config) when is_list(Config) ->
    {_,Host, Port} =  ?config(sftpd, Config),
    User = ?config(user, Config),
    Passwd = ?config(passwd, Config),
    {error,"server failed to start sftp subsystem"} =
	ssh_sftp:start_channel(Host, Port,
			       [{user_interaction, false},
				{user, User},
				{password, Passwd},
				{silently_accept_hosts, true}]).

%%--------------------------------------------------------------------
version_option()  ->
    [{doc, "Test API option sftp_vsn"}].
version_option(Config) when is_list(Config) ->
    open_close_dir(Config).

%%--------------------------------------------------------------------
create_empty_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write]),
    erl_tar:close(Handle),
    {ChPid,_} = ?config(sftp,Config),
    {ok, #file_info{type=regular}} =
	ssh_sftp:read_file_info(ChPid, TarFileName).

%%--------------------------------------------------------------------
files_to_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write]),
    F1 = ?config(tar_F1_txt, Config),
    ok = erl_tar:add(Handle, fn(F1,Config), F1, [verbose]),
    ok = erl_tar:add(Handle, fn("f2.txt",Config), "f2.txt", [verbose]),
    ok = erl_tar:close(Handle),
    chk_tar([F1, "f2.txt"], Config).

%%--------------------------------------------------------------------
big_file_to_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write]), 
    ok = erl_tar:add(Handle, fn("big.txt",Config), "big.txt", [verbose]),
    ok = erl_tar:close(Handle),
    chk_tar(["big.txt"], Config).


%%--------------------------------------------------------------------
files_chunked_to_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write]),
    F1 = ?config(tar_F1_txt, Config),
    ok = erl_tar:add(Handle, fn(F1,Config), F1, [verbose,{chunks,2}]),
    ok = erl_tar:close(Handle),
    chk_tar([F1], Config).

%%--------------------------------------------------------------------
directory_to_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write]),
    ok = erl_tar:add(Handle, fn("d1",Config), "d1", [verbose]),
    ok = erl_tar:close(Handle),
    chk_tar(["d1"], Config).
    
%%--------------------------------------------------------------------
binaries_to_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write]),
    Bin = <<"A binary">>,
    ok = erl_tar:add(Handle, Bin, "b1", [verbose]),
    ok = erl_tar:close(Handle),
    chk_tar([{"b1",Bin}], Config).

%%--------------------------------------------------------------------
null_crypto_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    Cinit = fun() -> {ok, no_state, _SendSize=5} end,
    Cenc = fun(Bin,CState) -> {ok,Bin,CState,_SendSize=5} end,
    Cend = fun(Bin,_CState) -> {ok,Bin} end,
    C = {Cinit,Cenc,Cend},
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write,{crypto,C}]),
    Bin = <<"A binary">>,
    F1 = ?config(tar_F1_txt, Config),
    ok = erl_tar:add(Handle, Bin, "b1", [verbose]),
    ok = erl_tar:add(Handle, fn(F1,Config), F1, [verbose,{chunks,2}]),
    ok = erl_tar:add(Handle, fn("big.txt",Config), "big.txt", [verbose,{chunks,15000}]),
    ok = erl_tar:close(Handle),
    chk_tar([{"b1",Bin}, F1, "big.txt"], Config).

%%--------------------------------------------------------------------
simple_crypto_tar_small(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    Cinit = fun() -> {ok, no_state, _Size=6} end,
    Cenc = fun(Bin,CState) -> {ok,stuff(Bin),CState,_SendSize=5} end,
    Cdec = fun(Bin,CState) -> {ok,unstuff(Bin),CState,_Size=4} end,
    Cend = fun(Bin,_CState) -> {ok,stuff(Bin)} end,
    C = {Cinit,Cenc,Cend},
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write,{crypto,C}]),
    Bin = <<"A binary">>,
    F1 = ?config(tar_F1_txt, Config),
    ok = erl_tar:add(Handle, Bin, "b1", [verbose]),
    ok = erl_tar:add(Handle, fn(F1,Config), F1, [verbose,{chunks,2}]),
    ok = erl_tar:close(Handle),
    chk_tar([{"b1",Bin}, F1], Config, [{crypto,{Cinit,Cdec}}]).

%%--------------------------------------------------------------------
simple_crypto_tar_big(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    Cinit = fun() -> {ok, no_state, _SendSize=6} end,
    Cenc = fun(Bin,CState) -> {ok,stuff(Bin),CState,_SendSize=5} end,
    Cdec = fun(Bin,CState) -> {ok,unstuff(Bin),CState,_SendSize=4} end,
    Cend = fun(Bin,_CState) -> {ok,stuff(Bin)} end,
    C = {Cinit,Cenc,Cend},
    TarFileName = ?config(tar_filename, Config),
    {ok,Handle} = ssh_sftp:open_tar(ChPid2, TarFileName, [write,{crypto,C}]),
    Bin = <<"A binary">>,
    F1 = ?config(tar_F1_txt, Config),
    ok = erl_tar:add(Handle, Bin, "b1", [verbose]),
    ok = erl_tar:add(Handle, fn(F1,Config), F1, [verbose,{chunks,2}]),
    ok = erl_tar:add(Handle, fn("big.txt",Config), "big.txt", [verbose,{chunks,15000}]),
    ok = erl_tar:close(Handle),
    chk_tar([{"b1",Bin}, F1, "big.txt"], Config, [{crypto,{Cinit,Cdec}}]).

stuff(Bin) -> << <<C,C>> || <<C>> <= Bin >>.
    
unstuff(Bin) -> << <<C>> || <<C,C>> <= Bin >>.
     
%%--------------------------------------------------------------------
read_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    NameBins = lists:sort(
		 [{"b1",<<"A binary">>},
		  {"b2",list_to_binary(lists:duplicate(750000,"a"))}
		 ]),
    TarFileName = ?config(tar_filename, Config),
    {ok,HandleWrite} = ssh_sftp:open_tar(ChPid2, TarFileName, [write]),
    [ok = erl_tar:add(HandleWrite, Bin, Name, [verbose])
     ||	{Name,Bin} <- NameBins],
    ok = erl_tar:close(HandleWrite),

    chk_tar(NameBins, Config).

%%--------------------------------------------------------------------
read_null_crypto_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    NameBins = lists:sort(
		 [{"b1",<<"A binary">>},
		  {"b2",list_to_binary(lists:duplicate(750000,"a"))}
		 ]),
    Cinitw = fun() -> {ok, no_state, _SendSize=5} end,
    Cinitr = fun() -> {ok, no_state, _FetchSize=42} end,
    Cenc = fun(Bin,CState) -> {ok,Bin,CState,_SendSize=42*42} end,
    Cdec = fun(Bin,CState) -> {ok,Bin,CState,_FetchSize=19} end,
    Cendw = fun(Bin,_CState) -> {ok,Bin} end,
    Cw = {Cinitw,Cenc,Cendw},
    Cr = {Cinitr,Cdec},

    TarFileName = ?config(tar_filename, Config),
    {ok,HandleWrite} = ssh_sftp:open_tar(ChPid2, TarFileName, [write,{crypto,Cw}]),
    [ok = erl_tar:add(HandleWrite, Bin, Name, [verbose])
     ||	{Name,Bin} <- NameBins],
    ok = erl_tar:close(HandleWrite),

    chk_tar(NameBins, Config, [{crypto,Cr}]).

%%--------------------------------------------------------------------
read_crypto_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    NameBins = lists:sort(
		 [{"b1",<<"A binary">>},
		  {"b2",list_to_binary(lists:duplicate(750000,"a"))}
		 ]),
    Cinitw = fun() -> {ok, no_state, _SendSize=5} end,
    Cinitr = fun() -> {ok, no_state, _FetchSize=42} end,

    Cenc = fun(Bin,CState) -> {ok,stuff(Bin),CState,_SendSize=42*42} end,
    Cdec = fun(Bin,CState) -> {ok,unstuff(Bin),CState,_FetchSize=120} end,
    Cendw = fun(Bin,_CState) -> {ok,stuff(Bin)} end,
    Cw = {Cinitw,Cenc,Cendw},
    Cr = {Cinitr,Cdec},

    TarFileName = ?config(tar_filename, Config),
    {ok,HandleWrite} = ssh_sftp:open_tar(ChPid2, TarFileName, [write,{crypto,Cw}]),
    [ok = erl_tar:add(HandleWrite, Bin, Name, [verbose])
     ||	{Name,Bin} <- NameBins],
    ok = erl_tar:close(HandleWrite),

    chk_tar(NameBins, Config, [{crypto,Cr}]).

%%--------------------------------------------------------------------
aes_cbc256_crypto_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    NameBins = lists:sort(
		 [{"b1",<<"A binary">>},
		  {"b2",list_to_binary(lists:duplicate(750000,"a"))},
		  {"d1",fn("d1",Config)}  % Dir
		 ]),
    Key = <<"This is a 256 bit key. Boring...">>,
    Ivec0 = crypto:rand_bytes(16),
    DataSize = 1024,  % data_size rem 16 = 0 for aes_cbc

    Cinitw = fun() -> {ok, Ivec0, DataSize} end,
    Cinitr = fun() -> {ok, Ivec0, DataSize} end,

    Cenc = fun(PlainBin,Ivec) -> 
		   CipherBin = crypto:block_encrypt(aes_cbc256, Key, Ivec, PlainBin),
		   {ok, CipherBin, crypto:next_iv(aes_cbc,CipherBin), DataSize}
	   end,
    Cdec = fun(CipherBin,Ivec) ->
		   PlainBin = crypto:block_decrypt(aes_cbc256, Key, Ivec, CipherBin),
		   {ok, PlainBin, crypto:next_iv(aes_cbc,CipherBin), DataSize}
	   end,

    Cendw = fun(PlainBin, _) when PlainBin == <<>> -> {ok, <<>>};
	       (PlainBin, Ivec) ->
		    CipherBin = crypto:block_encrypt(aes_cbc256, Key, Ivec, 
						     pad(16,PlainBin)), %% Last chunk
		    {ok, CipherBin} 
	    end,

    Cw = {Cinitw,Cenc,Cendw},
    TarFileName = ?config(tar_filename, Config),
    {ok,HandleWrite} = ssh_sftp:open_tar(ChPid2, TarFileName, [write,{crypto,Cw}]),
    [ok = erl_tar:add(HandleWrite, Bin, Name, [verbose]) || {Name,Bin} <- NameBins],
    ok = erl_tar:close(HandleWrite),

    Cr = {Cinitr,Cdec},
    chk_tar(NameBins, Config, [{crypto,Cr}]).


pad(BlockSize, Bin) ->
    PadSize = (BlockSize - (size(Bin) rem BlockSize)) rem BlockSize,
    list_to_binary( lists:duplicate(PadSize,0) ).

%%--------------------------------------------------------------------
aes_ctr_stream_crypto_tar(Config) ->
    ChPid2 = ?config(channel_pid2, Config),
    NameBins = lists:sort(
		 [{"b1",<<"A binary">>},
		  {"b2",list_to_binary(lists:duplicate(750000,"a"))},
		  {"d1",fn("d1",Config)}  % Dir
		 ]),
    Key = <<"This is a 256 bit key. Boring...">>,
    Ivec0 = crypto:rand_bytes(16),

    Cinitw = Cinitr = fun() -> {ok, crypto:stream_init(aes_ctr,Key,Ivec0)} end,

    Cenc = fun(PlainBin,State) -> 
		   {NewState,CipherBin} = crypto:stream_encrypt(State, PlainBin),
		   {ok, CipherBin, NewState}
	   end,
    Cdec = fun(CipherBin,State) ->
		   {NewState,PlainBin} = crypto:stream_decrypt(State, CipherBin),
		   {ok, PlainBin, NewState}
	   end,

    Cendw = fun(PlainBin, _) when PlainBin == <<>> -> {ok, <<>>};
	       (PlainBin, Ivec) ->
		    CipherBin = crypto:block_encrypt(aes_cbc256, Key, Ivec, 
						     pad(16,PlainBin)), %% Last chunk
		    {ok, CipherBin} 
	    end,

    Cw = {Cinitw,Cenc,Cendw},
    TarFileName = ?config(tar_filename, Config),
    {ok,HandleWrite} = ssh_sftp:open_tar(ChPid2, TarFileName, [write,{crypto,Cw}]),
    [ok = erl_tar:add(HandleWrite, Bin, Name, [verbose]) || {Name,Bin} <- NameBins],
    ok = erl_tar:close(HandleWrite),

    Cr = {Cinitr,Cdec},
    chk_tar(NameBins, Config, [{crypto,Cr}]).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
prep(Config) ->
    DataDir =  ?config(data_dir, Config),
    TestFile = ?config(filename, Config),
    TestFile1 = ?config(testfile, Config),
    TestLink = ?config(linktest, Config),
    TarFileName = ?config(tar_filename, Config),

    file:delete(TestFile),
    file:delete(TestFile1),
    file:delete(TestLink),
    file:delete(TarFileName),

    %% Initial config
    FileName = filename:join(DataDir, "sftp.txt"),
    file:copy(FileName, TestFile),
    Mode = 8#00400 bor 8#00200 bor 8#00040, % read & write owner, read group
    {ok, FileInfo} = file:read_file_info(TestFile),
    ok = file:write_file_info(TestFile,
			      FileInfo#file_info{mode = Mode}).



chk_tar(Items, Config) ->
    chk_tar(Items, Config, []).

chk_tar(Items, Config, Opts) ->
    TarFileName = ?config(tar_filename, Config),
    chk_tar(Items, TarFileName, Config, Opts).

chk_tar(Items, TarFileName, Config, Opts)  when is_list(Opts) ->
    tar_size(TarFileName, Config),
    {ChPid,_} = ?config(sftp,Config),
    {ok,HandleRead} = ssh_sftp:open_tar(ChPid, TarFileName, [read|Opts]),
    {ok,NameValueList} = erl_tar:extract(HandleRead,[memory,verbose]),
    ok = erl_tar:close(HandleRead),
    case {lists:sort(expand_items(Items,Config)), lists:sort(NameValueList)} of
	{L,L} -> 
	    true;
	{Expect,Actual} -> 
	    ct:log("Expect: ~p",[Expect]), ct:log("Actual: ~p",[Actual]),
	    case erl_tar:table(TarFileName) of
		{ok,Names} -> ct:log("names: ~p",[Names]);
		Other -> ct:log("~p",[Other])
	    end,
	    ct:log("~s",[analyze_report(Expect, Actual)]),
	    ct:fail(bad_tar_contents)
    end.

analyze_report([E={NameE,BinE}|Es], [A={NameA,BinA}|As]) ->
    if
	NameE == NameA,
	BinE =/= BinA-> 
	    [["Component ",NameE," differs. \n  Expected: ",BinE,"\n  Actual: ",BinA,"\n\n"]
	     | analyze_report(Es,As)];

	NameE < NameA ->
	    [["Component ",NameE," is missing.\n\n"]
	     | analyze_report(Es,[A|As])];
	
	NameE > NameA ->
	    [["Component ",NameA," is not expected.\n\n"]
	     | analyze_report([E|Es],As)];
	true ->
	    analyze_report(Es, As)
    end;
analyze_report([{NameE,_BinE}|Es], []) ->
    [["Component ",NameE," missing.\n\n"] | analyze_report(Es,[])];
analyze_report([], [{NameA,_BinA}|As]) ->
    [["Component ",NameA," not expected.\n\n"] | analyze_report([],As)];
analyze_report([], []) ->
    "".
	
tar_size(TarFileName, Config) ->
    {ChPid,_} = ?config(sftp,Config),
    {ok,Data} = ssh_sftp:read_file(ChPid, TarFileName),
    io:format('Tar file ~p is~n ~p bytes.~n',[TarFileName, size(Data)]).

expand_items(Items, Config) ->    
    lists:flatten(
      [case Item of
	   {_Name,Bin} when is_binary(Bin) -> 
	       Item;
	   {Name,FileName} when is_list(FileName) ->
	       read_item_contents(Name, fn(FileName,Config));
	   FileName  when is_list(FileName) ->
	       read_item_contents(FileName, fn(FileName,Config))
       end || Item <- Items]).

read_item_contents(ItemName, FileName) ->
    case file:read_file(FileName) of
	{ok,Bin} ->
	    {ItemName, Bin};
	{error,eisdir} ->
	    {ok,FileNames} = file:list_dir(FileName),
	    [read_item_contents(filename:join(ItemName,Name),
				filename:join(FileName,Name)) 
	     || Name<-FileNames]
    end.

fn(Name, Config) ->
    Dir = ?config(datadir_tar, Config),
    filename:join(Dir,Name).

fmt_host({A,B,C,D}) -> lists:concat([A,".",B,".",C,".",D]);
fmt_host(S) -> S.
    
