%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2013. All Rights Reserved.
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
-module(ssh_sftp_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

% Default timetrap timeout
-define(default_timeout, ?t:minutes(1)).

-define(USER, "Alladin").
-define(PASSWD, "Sesame").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, erlang_server},
     {group, openssh_server},
     sftp_nonexistent_subsystem
    ].


init_per_suite(Config) ->
    case (catch crypto:start()) of
	ok ->
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
    [{erlang_server, [], [open_close_file, open_close_dir, read_file, read_dir,
			  write_file, rename_file, mk_rm_dir, remove_file, links,
			  retrieve_attributes, set_attributes, async_read,
			  async_write, position, pos_read, pos_write]},
     {openssh_server, [], [open_close_file, open_close_dir, read_file, read_dir,
			   write_file, rename_file, mk_rm_dir, remove_file, links,
			   retrieve_attributes, set_attributes, async_read,
			   async_write, position, pos_read, pos_write]}].
     
init_per_group(erlang_server, Config) ->
    PrivDir = ?config(priv_dir, Config),
    SysDir =  ?config(data_dir, Config),
    Sftpd = 		       
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, PrivDir},
			     {user_passwords,
			      [{?USER, ?PASSWD}]}]),
    [{group, erlang_server}, {sftpd, Sftpd} | Config];

init_per_group(openssh_server, Config) ->
    Host = ssh_test_lib:hostname(),
    case (catch ssh_sftp:start_channel(Host,					      
				       [{user_interaction, false},
					{silently_accept_hosts, true}])) of
	{ok, _ChannelPid, Connection} ->
	    ssh:close(Connection),
	    [{group, openssh_server} | Config];
	_ ->
	    {skip, "No openssh server"} 
    end.

end_per_group(erlang_server, Config) ->
    Config;
end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------

init_per_testcase(sftp_nonexistent_subsystem, Config) ->
    PrivDir = ?config(priv_dir, Config),
    SysDir =  ?config(data_dir, Config),
    Sftpd = ssh_test_lib:daemon([{system_dir, SysDir},
				 {user_dir, PrivDir},
				 {subsystems, []},
				 {user_passwords,
				  [{?USER, ?PASSWD}]}
				]),
    [{sftpd, Sftpd} | Config];

init_per_testcase(Case, Config) ->
    prep(Config),
    TmpConfig0 = lists:keydelete(watchdog, 1, Config),
    TmpConfig = lists:keydelete(sftp, 1, TmpConfig0),
    Dog = ct:timetrap(?default_timeout),

    case ?config(group, Config) of 
	erlang_server ->
	    {_,Host, Port} =  ?config(sftpd, Config),
	    {ok, ChannelPid, Connection}  = 
		ssh_sftp:start_channel(Host, Port,
				       [{user, ?USER},
					{password, ?PASSWD},
					{user_interaction, false},
					{silently_accept_hosts, true}]),
	    Sftp = {ChannelPid, Connection},
	    [{sftp, Sftp}, {watchdog, Dog} | TmpConfig];
	openssh_server when Case == links ->
	    {skip, "known bug in openssh"};
	openssh_server ->
	    Host = ssh_test_lib:hostname(),
	    {ok, ChannelPid, Connection} = 
		ssh_sftp:start_channel(Host, 
				       [{user_interaction, false},
					{silently_accept_hosts, true}]), 
	    Sftp = {ChannelPid, Connection},
	    [{sftp, Sftp}, {watchdog, Dog} | TmpConfig]
    end.

end_per_testcase(sftp_nonexistent_subsystem, Config) ->
    Config;
end_per_testcase(rename_file, Config) ->
    PrivDir = ?config(priv_dir, Config),
    NewFileName = filename:join(PrivDir, "test.txt"),
    file:delete(NewFileName),  
    end_per_testcase(Config);
end_per_testcase(_, Config) ->
    end_per_testcase(Config).

end_per_testcase(Config) ->
    {Sftp, Connection} = ?config(sftp, Config),
    ssh_sftp:stop_channel(Sftp),
    ssh:close(Connection).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
open_close_file() ->
    [{doc, "Test API functions open/3 and close/2"}].
open_close_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),

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
    FileName = filename:join(PrivDir, "sftp.txt"),

    {ok, Handle} = ssh_sftp:opendir(Sftp, PrivDir),
    ok = ssh_sftp:close(Sftp, Handle),
    {error, _} =  ssh_sftp:opendir(Sftp, FileName).

%%--------------------------------------------------------------------
read_file() ->
    [{doc, "Test API funtion read_file/2"}].
read_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),
    {Sftp, _} = ?config(sftp, Config),
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
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),
    {Sftp, _} = ?config(sftp, Config),

    Data = list_to_binary("Hej hopp!"),
    ssh_sftp:write_file(Sftp, FileName, [Data]),
    {ok, Data} = file:read_file(FileName).

%%--------------------------------------------------------------------
remove_file() ->
    [{doc,"Test API function delete/2"}].
remove_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),
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
    FileName = filename:join(PrivDir, "sftp.txt"),
    NewFileName = filename:join(PrivDir, "test.txt"),

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
	    PrivDir =  ?config(priv_dir, Config),
	    FileName = filename:join(PrivDir, "sftp.txt"),
	    LinkFileName = filename:join(PrivDir, "link_test.txt"),

	    ok = ssh_sftp:make_symlink(Sftp, LinkFileName, FileName),
	    {ok, FileName} = ssh_sftp:read_link(Sftp, LinkFileName)
    end.

%%--------------------------------------------------------------------
retrieve_attributes() ->
    [{doc, "Test API function read_file_info/3"}].
retrieve_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),

    {Sftp, _} = ?config(sftp, Config),
    {ok, FileInfo} = ssh_sftp:read_file_info(Sftp, FileName),
    {ok, NewFileInfo} = file:read_file_info(FileName),

    %% TODO comparison. There are some differences now is that ok?
    ct:pal("SFTP: ~p   FILE: ~p~n", [FileInfo, NewFileInfo]).

%%--------------------------------------------------------------------
set_attributes() ->
    [{doc,"Test API function write_file_info/3"}].
set_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),

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
    PrivDir =  ?config(priv_dir, Config),

    FileName = filename:join(PrivDir, "sftp.txt"),
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
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
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
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
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
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
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
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
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
    {error,"server failed to start sftp subsystem"} =
	ssh_sftp:start_channel(Host, Port,
			       [{user_interaction, false},
				{user, ?USER}, {password, ?PASSWD},
				{silently_accept_hosts, true}]).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
prep(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    TestFile = filename:join(PrivDir, "sftp.txt"),
    TestFile1 = filename:join(PrivDir, "test.txt"),
    TestLink = filename:join(PrivDir, "link_test.txt"),

    file:delete(TestFile),
    file:delete(TestFile1),
    file:delete(TestLink),

    %% Initial config
    DataDir = ?config(data_dir, Config),
    FileName = filename:join(DataDir, "sftp.txt"),
    file:copy(FileName, TestFile),
    Mode = 8#00400 bor 8#00200 bor 8#00040, % read & write owner, read group
    {ok, FileInfo} = file:read_file_info(TestFile),
    ok = file:write_file_info(TestFile,
			      FileInfo#file_info{mode = Mode}).
