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

%% gerl +fnu
%% ct:run_test([{suite,"ssh_unicode_SUITE"}, {logdir,"LOG"}]).

-module(ssh_unicode_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

% Default timetrap timeout
-define(default_timeout, ?t:minutes(1)).

-define(USER, "åke高兴").
-define(PASSWD, "ärlig日本じん").
-define('sftp.txt',      "sftp瑞点.txt").
-define('test.txt',      "testハンス.txt").
-define('link_test.txt', "link_test語.txt").

-define(bindata, unicode:characters_to_binary("foobar å 一二三四いちにさんち") ).

-define(NEWLINE, <<"\r\n">>).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

%% suite() ->
%%     [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, sftp},
     {group, shell}
    ].


init_per_suite(Config) ->
    catch crypto:stop(),
    case {file:native_name_encoding(), (catch crypto:start())} of
	{utf8, ok} ->
	    ssh:start(),
	    Config;
	{utf8, _} ->
	    {skip,"Could not start crypto!"};
	_ ->
	    {skip,"Not unicode filename enabled emulator"}
    end.

end_per_suite(Config) ->
    ssh:stop(),
    crypto:stop(),
    Config.

%%--------------------------------------------------------------------
groups() -> 
    [{shell, [], [shell_no_unicode, shell_unicode_string]},
     {sftp, [], [open_close_file, open_close_dir, read_file, read_dir,
		 write_file, rename_file, mk_rm_dir, remove_file, links,
		 retrieve_attributes, set_attributes, async_read, async_read_bin,
		 async_write
		 %% , position, pos_read, pos_write
		]}].
     
init_per_group(Group, Config) when Group==sftp 
				 ; Group==shell ->
    PrivDir = ?config(priv_dir, Config),
    SysDir =  ?config(data_dir, Config),
    Sftpd = 		       
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, PrivDir},
			     {user_passwords, [{?USER, ?PASSWD}]}]),
    [{group,Group}, {sftpd, Sftpd} | Config];

init_per_group(Group, Config) ->
    [{group,Group} | Config].


end_per_group(erlang_server, Config) ->
    Config;
end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    prep(Config),
    TmpConfig0 = lists:keydelete(watchdog, 1, Config),
    TmpConfig = lists:keydelete(sftp, 1, TmpConfig0),
    Dog = ct:timetrap(?default_timeout),

    case ?config(group, Config) of 
	sftp ->
	    {_Pid, Host, Port} =  ?config(sftpd, Config),
	    {ok, ChannelPid, Connection}  = 
		ssh_sftp:start_channel(Host, Port,
				       [{user, ?USER},
					{password, ?PASSWD},
					{user_interaction, false},
					{silently_accept_hosts, true}]),
	    Sftp = {ChannelPid, Connection},
	    [{sftp, Sftp}, {watchdog, Dog} | TmpConfig];
	shell ->
	    UserDir = ?config(priv_dir, Config),
	    process_flag(trap_exit, true),
	    {_Pid, _Host, Port} =  ?config(sftpd, Config),
	    ct:sleep(500),
	    IO = ssh_test_lib:start_io_server(),
	    Shell = ssh_test_lib:start_shell(Port, IO, UserDir,
					     [{silently_accept_hosts, true},
					      {user,?USER},{password,?PASSWD}]),
%%ct:pal("IO=~p, Shell=~p, self()=~p",[IO,Shell,self()]),
	    wait_for_erlang_first_line([{io,IO}, {shell,Shell} | Config])
    end.


wait_for_erlang_first_line(Config) ->
    receive
	{'EXIT', _, _} ->
	    {fail,no_ssh_connection};
	<<"Eshell ",_/binary>> = ErlShellStart ->
%%	    ct:pal("Erlang shell start: ~p~n", [ErlShellStart]),
	    Config;
	Other ->
	    ct:pal("Unexpected answer from ssh server: ~p",[Other]),
	    {fail,unexpected_answer}
    after 10000 ->
	    ct:pal("No answer from ssh-server"),
	    {fail,timeout}
    end.



end_per_testcase(rename_file, Config) ->
    PrivDir = ?config(priv_dir, Config),
    NewFileName = filename:join(PrivDir, ?'test.txt'),
    file:delete(NewFileName),  
    end_per_testcase(Config);
end_per_testcase(_TC, Config) ->
    end_per_testcase(Config).

end_per_testcase(Config) ->
    catch exit(?config(shell,Config), kill),
    case  ?config(sftp, Config) of
	{Sftp, Connection} ->
	    ssh_sftp:stop_channel(Sftp),
	    ssh:close(Connection);
	_ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------

-define(chk_expected(Received,Expected),
	(fun(R_,E_) when R_==E_ -> ok;
	    (R_,E_) -> ct:pal("Expected: ~p~nReceived: ~p~n", [E_,R_]),
		       E_ = R_
	 end)(Received,Expected)).

-define(receive_chk(Ref,Expected),
	(fun(E__) ->
		 receive
		     {async_reply, Ref, Received} when Received==E__ ->
			 ?chk_expected(Received, E__);
		     {async_reply, Ref, Received} when Received=/=E__ ->
			 ct:pal("Expected: ~p~nReceived: ~p~n", [E__,Received]),
			 E__ = Received;
		     Msg ->
			 ct:pal("Expected (Ref=~p): ~p", [Ref,E__]),
			 ct:fail(Msg)
		 end
	 end)(Expected)).

%%--------------------------------------------------------------------


open_close_file() ->
    [{doc, "Test API functions open/3 and close/2"}].
open_close_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'sftp.txt'),
    {Sftp, _} = ?config(sftp, Config),

    lists:foreach(
      fun(Mode) ->
	      ct:log("Mode: ~p",[Mode]),
	      %% list_dir(PrivDir),
	      ok = open_close_file(Sftp, FileName, Mode)
      end,
      [
       [read],
       [write],
       [write, creat],
       [write, trunc],
       [append],
       [read, binary]
      ]).

open_close_file(Server, File, Mode) ->
    {ok, Handle} = ssh_sftp:open(Server, File, Mode),
    ok = ssh_sftp:close(Server, Handle).

%%--------------------------------------------------------------------
open_close_dir() ->
    [{doc, "Test API functions opendir/2 and close/2"}].
open_close_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
    FileName = filename:join(PrivDir, ?'sftp.txt'),

    {ok, Handle} = ssh_sftp:opendir(Sftp, PrivDir),
    ok = ssh_sftp:close(Sftp, Handle),
    {error, _} =  ssh_sftp:opendir(Sftp, FileName).

%%--------------------------------------------------------------------
read_file() ->
    [{doc, "Test API funtion read_file/2"}].
read_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'sftp.txt'),
    {Sftp, _} = ?config(sftp, Config),
    ?chk_expected(ssh_sftp:read_file(Sftp,FileName),  file:read_file(FileName)).

%%--------------------------------------------------------------------
read_dir() ->
    [{doc,"Test API function list_dir/2"}].
read_dir(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {Sftp, _} = ?config(sftp, Config),
    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),
    ct:pal("sftp list dir: ~ts~n", [Files]).

%%--------------------------------------------------------------------
write_file() ->
    [{doc, "Test API function write_file/2"}].
write_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'sftp.txt'),
    {Sftp, _} = ?config(sftp, Config),
    ok = ssh_sftp:write_file(Sftp, FileName, [?bindata]),
    ?chk_expected(file:read_file(FileName), {ok,?bindata}).

%%--------------------------------------------------------------------
remove_file() ->
    [{doc,"Test API function delete/2"}].
remove_file(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'sftp.txt'),
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
    FileName = filename:join(PrivDir, ?'sftp.txt'),
    NewFileName = filename:join(PrivDir, ?'test.txt'),

    {Sftp, _} = ?config(sftp, Config),
    {ok, Files} = ssh_sftp:list_dir(Sftp, PrivDir),
    ct:pal("FileName: ~ts~nFiles: ~ts~n", [FileName, [[$\n,$ ,F]||F<-Files] ]),
    true = lists:member(filename:basename(FileName), Files),
    false = lists:member(filename:basename(NewFileName), Files),
    ok = ssh_sftp:rename(Sftp, FileName, NewFileName),
    {ok, NewFiles} = ssh_sftp:list_dir(Sftp, PrivDir),
    ct:pal("FileName: ~ts, Files: ~ts~n", [FileName, [[$\n,F]||F<-NewFiles] ]),

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
	    FileName = filename:join(PrivDir, ?'sftp.txt'),
	    LinkFileName = filename:join(PrivDir, ?'link_test.txt'),

	    ok = ssh_sftp:make_symlink(Sftp, LinkFileName, FileName),
	    {ok, FileName} = ssh_sftp:read_link(Sftp, LinkFileName)
    end.

%%--------------------------------------------------------------------
retrieve_attributes() ->
    [{doc, "Test API function read_file_info/3"}].
retrieve_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'sftp.txt'),

    {Sftp, _} = ?config(sftp, Config),
    {ok, FileInfo} = ssh_sftp:read_file_info(Sftp, FileName),
    {ok, NewFileInfo} = file:read_file_info(FileName),

    %% TODO comparison. There are some differences now is that ok?
    ct:pal("SFTP: ~p~nFILE: ~p~n", [FileInfo, NewFileInfo]).

%%--------------------------------------------------------------------
set_attributes() ->
    [{doc,"Test API function write_file_info/3"}].
set_attributes(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'test.txt'),

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
    do_async_read(Config, false).

async_read_bin() ->
    [{doc,"Test API aread/3"}].
async_read_bin(Config) when is_list(Config) ->
    do_async_read(Config, true).

do_async_read(Config, BinaryFlag) ->
    {Sftp, _} = ?config(sftp, Config),
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'sftp.txt'),
    {ok,ExpDataBin} = file:read_file(FileName),
    ExpData = case BinaryFlag of
		  true -> ExpDataBin;
		  false -> binary_to_list(ExpDataBin)
	      end,
    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [read|case BinaryFlag of
							   true -> [binary];
							   false -> []
						       end]),
    {async, Ref} = ssh_sftp:aread(Sftp, Handle, 20),
    ?receive_chk(Ref, {ok,ExpData}).

%%--------------------------------------------------------------------
async_write() ->
    [{doc,"Test API awrite/3"}].
async_write(Config) when is_list(Config) ->
    {Sftp, _} = ?config(sftp, Config),
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'test.txt'),
    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [write]),
    Expected = ?bindata,
    {async, Ref} = ssh_sftp:awrite(Sftp, Handle, Expected),

    receive
	{async_reply, Ref, ok} ->
	    {ok, Data} = file:read_file(FileName),
	    ?chk_expected(Data, Expected);
	Msg ->
	    ct:fail(Msg)
    end.

%%--------------------------------------------------------------------

position() ->
    [{doc, "Test API functions position/3"}].
position(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'test.txt'),
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
    FileName = filename:join(PrivDir, ?'test.txt'),
    {Sftp, _} = ?config(sftp, Config),
    Data = ?bindata,
    ssh_sftp:write_file(Sftp, FileName, [Data]),

    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [read]),
    {async, Ref} = ssh_sftp:apread(Sftp, Handle, {bof,5}, 4),

    ?receive_chk(Ref, {ok,binary_part(Data,5,4)}),
    ?chk_expected(ssh_sftp:pread(Sftp,Handle,{bof,4},4), {ok,binary_part(Data,4,4)}).


%%--------------------------------------------------------------------
pos_write() ->
    [{doc,"Test API functions pwrite/4 and apwrite/4"}].
pos_write(Config) when is_list(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    FileName = filename:join(PrivDir, ?'test.txt'),
    {Sftp, _} = ?config(sftp, Config),

    {ok, Handle} = ssh_sftp:open(Sftp, FileName, [write]),

    Data = unicode:characters_to_list("再见"),
    ssh_sftp:write_file(Sftp, FileName, [Data]),

    NewData = unicode:characters_to_list("  さようなら"),
    {async, Ref} = ssh_sftp:apwrite(Sftp, Handle, {bof, 2}, NewData),
    ?receive_chk(Ref, ok),

    ok = ssh_sftp:pwrite(Sftp, Handle, eof, unicode:characters_to_list(" adjö ")),

    ?chk_expected(ssh_sftp:read_file(Sftp,FileName), 
		  {ok,unicode:characters_to_binary("再见  さようなら adjö ")}).

%%--------------------------------------------------------------------
sftp_nonexistent_subsystem() ->
    [{doc, "Try to execute sftp subsystem on a server that does not support it"}].
sftp_nonexistent_subsystem(Config) when is_list(Config) ->
    {_,Host, Port} =  ?config(sftpd, Config),
    {error,"server failed to start sftp subsystem"} =
	ssh_sftp:start_channel(Host, Port,
			       [{user_interaction, false},
				{user, ?USER}, 
				{password, ?PASSWD},
				{silently_accept_hosts, true}]).

%%--------------------------------------------------------------------
shell_no_unicode(Config) ->
    do_shell(?config(io,Config),
	     [new_prompt,
 	      {type,"io:format(\"hej ~p~n\",[42])."},
	      {expect,"hej 42"}
	     ]).
	      
%%--------------------------------------------------------------------
shell_unicode_string(Config) ->
    do_shell(?config(io,Config),
	     [new_prompt,
 	      {type,"io:format(\"こにちわ~ts~n\",[\"四二\"])."},
	      {expect,"こにちわ四二"},
	      {expect,"ok"}
	     ]).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
prep(Config) ->
    PrivDir =  ?config(priv_dir, Config),
    TestFile = filename:join(PrivDir, ?'sftp.txt'),
    TestFile1 = filename:join(PrivDir, ?'test.txt'),
    TestLink = filename:join(PrivDir, ?'link_test.txt'),

    file:delete(TestFile),
    file:delete(TestFile1),
    file:delete(TestLink),

    %% Initial config
    DataDir = ?config(data_dir, Config),
    FileName = filename:join(DataDir, ?'sftp.txt'),
    {ok,_BytesCopied} = file:copy(FileName, TestFile),
    Mode = 8#00400 bor 8#00200 bor 8#00040, % read & write owner, read group
    {ok, FileInfo} = file:read_file_info(TestFile),
    ok = file:write_file_info(TestFile,
			      FileInfo#file_info{mode = Mode}).


%% list_dir(Dir) ->
%%     ct:pal("prep/1: ls(~p):~n~p~n~ts",[Dir, file:list_dir(Dir), 
%% 				       begin
%% 					   {ok,DL} = file:list_dir(Dir),
%% 					   [[$\n|FN] || FN <- DL]
%% 				       end]).
    

%%--------------------------------------------------------------------
do_shell(IO, List) -> do_shell(IO, 0, List).

do_shell(IO, N, [new_prompt|More]) ->
    do_shell(IO, N+1, More);

do_shell(IO, N, Ops=[{Order,Arg}|More]) ->
    receive
	X = <<"\r\n">> ->
%%	    ct:pal("Skip newline ~p",[X]),
	    do_shell(IO, N, Ops);
	
	<<P1,"> ">> when (P1-$0)==N -> 
	    do_shell_prompt(IO, N, Order, Arg, More);

	<<P1,P2,"> ">> when (P1-$0)*10 + (P2-$0) == N -> 
	    do_shell_prompt(IO, N, Order, Arg, More);

	Err when element(1,Err)==error ->
	    ct:fail("do_shell error: ~p~n",[Err]);

	RecBin when Order==expect ; Order==expect_echo ->
%%	    ct:pal("received ~p",[RecBin]),
	    RecStr = string:strip(unicode:characters_to_list(RecBin)),
	    ExpStr = string:strip(Arg),
	    case lists:prefix(ExpStr, RecStr) of
		true when Order==expect ->
		    ct:pal("Matched ~ts",[RecStr]),
		    do_shell(IO, N, More);
		true when Order==expect_echo ->
		    ct:pal("Matched echo ~ts",[RecStr]),
		    do_shell(IO, N, More);
		false ->
		    ct:fail("*** Expected ~p, but got ~p",[string:strip(ExpStr),RecStr])
	    end
    after 10000 ->
	    case Order of
		expect -> ct:fail("timeout, expected ~p",[string:strip(Arg)]);
		type ->  ct:fail("timeout, no prompt")
	    end
    end;

do_shell(_, _, []) ->
    ok.


do_shell_prompt(IO, N, type, Str, More) ->
%%    ct:pal("Matched prompt ~p to trigger sending of next line to server",[N]),
    IO ! {input, self(), Str++"\r\n"},
    ct:pal("Promt '~p> ', Sent ~ts",[N,Str++"\r\n"]),
    do_shell(IO, N, [{expect_echo,Str}|More]); % expect echo of the sent line
do_shell_prompt(IO, N, Op, Str, More) ->
%%    ct:pal("Matched prompt ~p",[N]),
    do_shell(IO, N, [{Op,Str}|More]).
  
%%--------------------------------------------------------------------
