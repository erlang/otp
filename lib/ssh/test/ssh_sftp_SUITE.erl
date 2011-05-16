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
-module(ssh_sftp_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").

-include_lib("kernel/include/file.hrl").

% Default timetrap timeout
-define(default_timeout, ?t:minutes(1)).

-define(SFPD_PORT, 9999).
-define(USER, "Alladin").
-define(PASSWD, "Sesame").

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
    case {catch crypto:start(),catch ssh:start()} of
	{ok,ok} ->
	    Dir = ?config(priv_dir, Config),
	    {ok, _} = ssh_test_lib:get_id_keys(Dir),
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
end_per_suite(Config) ->
    crypto:stop(),
    Dir = ?config(priv_dir, Config),
    ssh_test_lib:remove_id_keys(Dir),
    Config.

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
init_per_testcase(_Case, Config) ->
    prep(Config),
    TmpConfig0 = lists:keydelete(watchdog, 1, Config),
    TmpConfig = lists:keydelete(sftp, 1, TmpConfig0),
    Dog = test_server:timetrap(?default_timeout),
    Dir = ?config(priv_dir, Config),
    SysDir =  ?config(data_dir, Config),
    Host = ssh_test_lib:hostname(),

    Sftp = case (catch ssh_sftp:start_channel(Host,
					      [{user_dir, Dir},
					       {user_interaction, false},
					       {silently_accept_hosts, true}])) of
	       {ok, ChannelPid, Connection} ->
		   {ChannelPid, Connection};
	       _Error ->
		   {_Sftpd, _Host, _Port} = 
		       ssh_test_lib:daemon(Host, ?SFPD_PORT,
					   [{system_dir, SysDir},
					    {user_passwords,
					     [{?USER, ?PASSWD}]},
					    {failfun,
					     fun ssh_test_lib:failfun/2}]),
		   Result = (catch ssh_sftp:start_channel(Host, ?SFPD_PORT,
							  [{user, ?USER},
							   {password, ?PASSWD},
							   {user_interaction, false},
							   {silently_accept_hosts, true}])),
		   {ok, ChannelPid, Connection} = Result,
		   {ChannelPid, Connection}
	   end,

    [{sftp, Sftp}, {watchdog, Dog} | TmpConfig].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_Case, Config) ->
    {Sftp, Connection} = ?config(sftp, Config),
    ssh_sftp:stop_channel(Sftp),
    ssh:close(Connection),
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
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
     write_file, rename_file, mk_rm_dir, remove_file, links,
     retrieve_attributes, set_attributes, async_read,
     async_write, position, pos_read, pos_write].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


%% Test cases starts here.
%%--------------------------------------------------------------------
open_close_file(doc) ->
    ["Test API functions open/3 and close/2"];
open_close_file(suite) ->
    [];
open_close_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),

    {Sftp, _} = ?config(sftp, Config),

    ok = open_close_file(Sftp, FileName, [read]),
    ok = open_close_file(Sftp, FileName, [write]),
    ok = open_close_file(Sftp, FileName, [write, creat]),
    ok = open_close_file(Sftp, FileName, [write, trunc]),
    ok = open_close_file(Sftp, FileName, [append]),
    ok = open_close_file(Sftp, FileName, [read, binary]),

    ok.

open_close_file(Server, File, Mode) ->
    {ok, Handle} = ssh_sftp:open(Server, File, Mode),
    ok = ssh_sftp:close(Server, Handle),
    ok.


%%--------------------------------------------------------------------
open_close_dir(doc) ->
    ["Test API functions opendir/2 and close/2"];
open_close_dir(suite) ->
    [];
open_close_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),

    {ok, Handle} = ssh_sftp:opendir(Sftp, PrivDir),
    ok = ssh_sftp:close(Sftp, Handle),
    {error, _} =  ssh_sftp:opendir(Sftp, FileName),

    ok.
%%--------------------------------------------------------------------
read_file(doc) ->
    ["Test API funtion read_file/2"];
read_file(suite) ->
    [];
read_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),

    {Sftp, _} = ?config(sftp, Config),

    {ok, Data} = ssh_sftp:read_file(Sftp, FileName),

    {ok, Data} = file:read_file(FileName),

    ok.
%%--------------------------------------------------------------------
read_dir(doc) ->
    ["Test API function list_dir/2"];
read_dir(suite) ->
    [];
read_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),
    test_server:format("sftp list dir: ~p~n", [Files]),
    ok.

%%--------------------------------------------------------------------
write_file(doc) ->
    ["Test API function write_file/2"];
write_file(suite) ->
    [];
write_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),

    {Sftp, _} = ?config(sftp, Config),

    Data = list_to_binary("Hej hopp!"),

    ssh_sftp:write_file(Sftp, FileName, [Data]),

    {ok, Data} = file:read_file(FileName),

    ok.

%%--------------------------------------------------------------------
remove_file(doc) ->
    ["Test API function delete/2"];
remove_file(suite) ->
    [];
remove_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),

    {Sftp, _} = ?config(sftp, Config),

    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),

    true = lists:member(filename:basename(FileName), Files),

    ok = ssh_sftp:delete(Sftp, FileName),

    {ok, NewFiles} = ssh_sftp:list_dir(Sftp, PrivDir),

    false = lists:member(filename:basename(FileName), NewFiles),

    {error, _} = ssh_sftp:delete(Sftp, FileName),

    ok.

%%--------------------------------------------------------------------
rename_file(doc) ->
    ["Test API function rename_file/2"];
rename_file(suite) ->
    [];
rename_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),
    NewFileName = filename:join(PrivDir, "test.txt"),

    {Sftp, _} = ?config(sftp, Config),

    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),

    test_server:format("FileName: ~p, Files: ~p~n", [FileName, Files]),

    true = lists:member(filename:basename(FileName), Files),
    false = lists:member(filename:basename(NewFileName), Files),

    ok = ssh_sftp:rename(Sftp, FileName, NewFileName),

    {ok, NewFiles} = ssh_sftp:list_dir(Sftp, PrivDir),

    test_server:format("FileName: ~p, Files: ~p~n", [FileName, NewFiles]),

    false = lists:member(filename:basename(FileName), NewFiles),
    true = lists:member(filename:basename(NewFileName), NewFiles),

    ok.

%%--------------------------------------------------------------------
mk_rm_dir(doc) ->
    ["Test API functions make_dir/2, del_dir/2"];
mk_rm_dir(suite) ->
    [];
mk_rm_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
    DirName = filename:join(PrivDir, "test"),

    ok = ssh_sftp:make_dir(Sftp, DirName),
    ok = ssh_sftp:del_dir(Sftp, DirName),

    NewDirName = filename:join(PrivDir, "foo/bar"),

    {error, _} = ssh_sftp:make_dir(Sftp, NewDirName),
    {error, _} = ssh_sftp:del_dir(Sftp, PrivDir),

    ok.

%%--------------------------------------------------------------------
links(doc) ->
    ["Tests API function make_symlink/3"];
links(suite) ->
    [];
links(Config) when is_list(Config) ->
    case test_server:os_type() of
	{win32, _} ->
	    {skip, "Links are not fully supported by windows"};
	_ ->
	    {Sftp, _} = ?config(sftp, Config),
	    PrivDir =  ?config(priv_dir, Config),
	    FileName = filename:join(PrivDir, "sftp.txt"),
	    LinkFileName = filename:join(PrivDir, "link_test.txt"),

	    ok = ssh_sftp:make_symlink(Sftp, FileName, LinkFileName),
	    {ok, FileName} = ssh_sftp:read_link(Sftp, LinkFileName),
	    ok
    end.

%%--------------------------------------------------------------------
retrieve_attributes(doc) ->
    ["Test API function read_file_info/3"];
retrieve_attributes(suite) ->
    [];
retrieve_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),
    {Sftp, _} = ?config(sftp, Config),

    {ok, FileInfo} = ssh_sftp:read_file_info(Sftp, FileName),

    {ok, NewFileInfo} = file:read_file_info(FileName),

    %% TODO comparison. There are some differences now is that ok?
    test_server:format("SFTP: ~p   FILE: ~p~n", [FileInfo, NewFileInfo]),
    ok.

%%--------------------------------------------------------------------
set_attributes(doc) ->
    ["Test API function write_file_info/3"];
set_attributes(suite) ->
    [];
set_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "test.txt"),
    {Sftp, _} = ?config(sftp, Config),

    {ok,Fd} = file:open(FileName, write),
    io:put_chars(Fd,"foo"),

    ok = ssh_sftp:write_file_info(Sftp, FileName, #file_info{mode=8#400}),
    {error, eacces} = file:write_file(FileName, "hello again"),
    ssh_sftp:write_file_info(Sftp, FileName, #file_info{mode=8#600}),
    ok = file:write_file(FileName, "hello again"),

    ok.

%%--------------------------------------------------------------------

async_read(doc) ->
    ["Test API aread/3"];
async_read(suite) ->
    [];
async_read(Config) when is_list(Config) ->
    {Sftp, _} = ?config(sftp, Config),
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, "sftp.txt"),
    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [read]),
    {async, Ref} = ssh_sftp:aread(Sftp, Handle, 20),

    receive
	{async_reply, Ref, {ok, Data}} ->
	    test_server:format("Data: ~p~n", [Data]),
	    ok;
	Msg ->
	    test_server:fail(Msg)
    end,
    ok.
%%--------------------------------------------------------------------
async_write(doc) ->
    ["Test API awrite/3"];
async_write(suite) ->
    [];
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
	    test_server:fail(Msg)
    end,
    ok.

%%--------------------------------------------------------------------

position(doc) ->
    ["Test API functions position/3"];
position(suite) ->
    [];
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
    {ok, "2"} = ssh_sftp:read(Sftp, Handle, 1),

    ok.

%%--------------------------------------------------------------------
pos_read(doc) ->
    ["Test API functions pread/3 and apread/3"];
pos_read(suite) ->
    [];
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
	    test_server:fail(Msg)
    end,

    NewData1  = "hopp",

    {ok, NewData1} = ssh_sftp:pread(Sftp, Handle, {bof, 4}, 4),

    ok.
%%--------------------------------------------------------------------
pos_write(doc) ->
    ["Test API functions pwrite/4 and apwrite/4"];
pos_write(suite) ->
    [];
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
	    test_server:fail(Msg)
    end,

    ok = ssh_sftp:pwrite(Sftp, Handle, eof, list_to_binary("!")),

    NewData1 = list_to_binary("Bye, see you tomorrow!"),
    {ok, NewData1} = ssh_sftp:read_file(Sftp, FileName),

    ok.

%% Internal functions
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
