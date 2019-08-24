%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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
-module(ftp_SUITE).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(FTP_USER, "anonymous").
-define(FTP_PASS(Cmnt), (fun({ok,__H}) -> "ftp_SUITE_"++Cmnt++"@" ++ __H;
			    (_) -> "ftp_SUITE_"++Cmnt++"@localhost"
			 end)(inet:gethostname())
       ).

-define(BAD_HOST, "badhostname").
-define(BAD_USER, "baduser").
-define(BAD_DIR,  "baddirectory").

-record(progress, {
	  current = 0,
	  total
	 }).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,20}}].

all() ->
    [
     {group, ftp_passive},
     {group, ftp_active},
     {group, ftps_passive},
     {group, ftps_active},
     {group, ftp_sup},
     app,
     appup,
     error_ehost,
     clean_shutdown
    ].

groups() ->
    [
     {ftp_passive, [], ftp_tests()},
     {ftp_active, [], ftp_tests()},
     {ftps_passive, [], ftp_tests()},
     {ftps_active, [], ftp_tests()},
     {ftp_sup, [], ftp_sup_tests()}
    ].

ftp_tests()->
    [
     user,
     bad_user,
     pwd, 
     cd, 
     lcd,
     ls, 
     nlist, 
     rename, 
     delete, 
     mkdir, 
     rmdir,
     send, 
     send_3,
     send_bin, 
     send_chunk, 
     append, 
     append_bin,
     append_chunk, 
     recv, 
     recv_3, 
     recv_bin,
     recv_bin_twice,
     recv_chunk, 
     recv_chunk_twice,
     recv_chunk_three_times,
     recv_chunk_delay,
     type, 
     quote, 
     error_elogin,
     progress_report_send,
     progress_report_recv,
     not_owner,
     unexpected_call,
     unexpected_cast,
     unexpected_bang
    ].

ftp_sup_tests() ->
    [
     start_ftp,
     ftp_worker
    ].

%%--------------------------------------------------------------------

%%% Config
%%% key			meaning
%%% ................................................................
%%% ftpservers		list of servers to check if they are available
%%%			The element is:
%%%			  {Name,         % string(). The os command name
%%%                        Path,         % string(). The os PATH syntax, e.g "/bin:/usr/bin"
%%%			   StartCommand, % fun()->{ok,start_result()} | {error,string()}.
%%%			                 % The command to start the daemon with.
%%%			   ChkUp,        % fun(start_result()) -> string(). Os command to check
%%%			                 %       if the server is running. [] if not running.
%%%			                 %       The string in string() is suitable for logging.
%%%			   StopCommand,  % fun(start_result()) -> void(). The command to stop the daemon with.
%%%			   AugmentFun,   % fun(config()) -> config() Adds two funs for transforming names of files
%%%			                 %       and directories to the form they are returned from this server
%%%			   ServerHost,   % string(). Mostly "localhost"
%%%			   ServerPort    % pos_integer()
%%%			  }
%%%			  

-define(default_ftp_servers,
	[{"vsftpd",
	  "/sbin:/usr/sbin:/usr/local/sbin",
	  fun(__CONF__, AbsName) -> 
		  DataDir = proplists:get_value(data_dir,__CONF__),
		  ConfFile = filename:join(DataDir, "vsftpd.conf"),
		  PrivDir = proplists:get_value(priv_dir,__CONF__),
		  AnonRoot = PrivDir,
		  Cmd = [AbsName ++" "++filename:join(DataDir,"vsftpd.conf"),
			 " -oftpd_banner=erlang_otp_testing",
			 " -oanon_root=\"",AnonRoot,"\"",
			 " -orsa_cert_file=\"",filename:join(DataDir,"server-cert.pem"),"\"",
			 " -orsa_private_key_file=\"",filename:join(DataDir,"server-key.pem"),"\""
			],
		  Result = os:cmd(Cmd),
		  ct:log("Config file:~n~s~n~nServer start command:~n  ~s~nResult:~n  ~p",
			 [case file:read_file(ConfFile) of
			      {ok,X} -> X;
			      _ -> ""
			  end,
			  Cmd, Result
			 ]),
		  case Result of
		      [] -> {ok,'dont care'};
		      [Msg] -> {error,Msg}
		  end
	  end,
	  fun(_StartResult) -> os:cmd("ps ax | grep erlang_otp_testing | grep -v grep")
	  end,
	  fun(_StartResult) -> os:cmd("kill `ps ax | grep erlang_otp_testing | awk '/vsftpd/{print $1}'`")
	  end,
	  fun(__CONF__) ->
		  AnonRoot = proplists:get_value(priv_dir,__CONF__),
		  [{id2ftp, fun(Id) -> filename:join(AnonRoot,Id) end},
		   {id2ftp_result,fun(Id) -> filename:join(AnonRoot,Id) end} | __CONF__]
	  end,
	  "localhost",
	  9999
	 }
	]
       ).


init_per_suite(Config) ->
    case find_executable(Config) of
	false -> 
	    {skip, "No ftp server found"};
	{ok,Data} -> 
	    TstDir = filename:join(proplists:get_value(priv_dir,Config), "test"),
	    file:make_dir(TstDir),
	    %% make_cert_files(dsa, rsa, "server-", proplists:get_value(data_dir,Config)),
            ftp_test_lib:make_cert_files(proplists:get_value(data_dir,Config)),
	    start_ftpd([{test_dir,TstDir},
			{ftpd_data,Data}
			| Config])
    end.

end_per_suite(Config) ->
    ps_ftpd(Config),
    stop_ftpd(Config),
    ps_ftpd(Config),
    ok.

%%--------------------------------------------------------------------
init_per_group(Group, Config) when Group == ftps_active,
                                   Group == ftps_passive ->
    catch crypto:stop(),
    try crypto:start() of
        ok ->
            Config
    catch
        _:_ ->
            {skip, "Crypto did not start"}
    end;
init_per_group(ftp_sup, Config) ->
    try ftp:start() of
        ok ->
            Config
    catch
        _:_ ->
            {skip, "Ftp did not start"}
    end;
init_per_group(_Group, Config) -> 
    Config.


end_per_group(ftp_sup, Config) -> 
    ftp:stop(),
    Config;
end_per_group(_Group, Config) -> 
    Config.

%%--------------------------------------------------------------------
init_per_testcase(T, Config0) when T =:= app; T =:= appup ->
    Config0;
init_per_testcase(Case, Config0) ->
    Group = proplists:get_value(name, proplists:get_value(tc_group_properties,Config0)),

    %% Workaround for interoperability issues with vsftpd =< 3.0.2:
    %%
    %% vsftpd =< 3.0.2 does not support ECDHE ciphers and the ssl application
    %% removed ciphers with RSA key exchange from its default cipher list.
    %% To allow interoperability with old versions of vsftpd, cipher suites
    %% with RSA key exchange are appended to the default cipher list.
    All = ssl:cipher_suites(all, 'tlsv1.2'),
    Default = ssl:cipher_suites(default, 'tlsv1.2'),
    RSASuites =
        ssl:filter_cipher_suites(All, [{key_exchange, fun(rsa) -> true;
                                                         (_) -> false end}]),
    Suites = ssl:append_cipher_suites(RSASuites, Default),
    TLS = [{tls,[{reuse_sessions,true},{ciphers, Suites}]}],
    ACTIVE = [{mode,active}],
    PASSIVE = [{mode,passive}],
    CaseOpts = case Case of
		   progress_report_send -> [{progress, {?MODULE,progress,#progress{}}}];
		   progress_report_recv -> [{progress, {?MODULE,progress,#progress{}}}];
		   _ -> []
	       end,
    ExtraOpts = [verbose | CaseOpts],
    Config =
	case Group of
	    ftp_active   -> ftp__open(Config0,       ACTIVE  ++ ExtraOpts);
	    ftps_active  -> ftp__open(Config0, TLS++ ACTIVE  ++ ExtraOpts);
	    ftp_passive  -> ftp__open(Config0,      PASSIVE  ++ ExtraOpts);
	    ftps_passive -> ftp__open(Config0, TLS++PASSIVE  ++ ExtraOpts);
            ftp_sup      -> ftp_start_service(Config0, ACTIVE  ++ ExtraOpts);
	    undefined    -> Config0
	end,
    case Case of
	user           -> Config;
	bad_user       -> Config;
	error_elogin   -> Config;
	error_ehost    -> Config;
	clean_shutdown -> Config;
	_ ->
	    Pid = proplists:get_value(ftp,Config),
	    ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS(atom_to_list(Group)++"-"++atom_to_list(Case)) ),
	    ok = ftp:cd(Pid, proplists:get_value(priv_dir,Config)),
	    Config
    end.

end_per_testcase(T, _Config) when  T =:= app; T =:= appup -> ok;
end_per_testcase(user, _Config) -> ok;
end_per_testcase(bad_user, _Config) -> ok;
end_per_testcase(error_elogin, _Config) -> ok;
end_per_testcase(error_ehost, _Config) -> ok;
end_per_testcase(clean_shutdown, _Config) -> ok;
end_per_testcase(_Case, Config) -> 
    case proplists:get_value(tc_status,Config) of
	ok -> ok;
	_ ->
	    try ftp:latest_ctrl_response(proplists:get_value(ftp,Config))
	    of
		{ok,S} -> ct:log("***~n*** Latest ctrl channel response:~n***     ~p~n***",[S])
	    catch
		_:_ -> ok
	    end
    end,
    Group = proplists:get_value(name, proplists:get_value(tc_group_properties,Config)),
    case Group of
        ftp_sup ->
            ftp_stop_service(Config);
        _Else ->
            ftp__close(Config)
    end.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app() ->
    [{doc, "Test that the ftp app file is ok"}].
app(Config) when is_list(Config) ->
    ok = ?t:app_test(ftp).

%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the ftp appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(ftp).

%%--------------------------------------------------------------------

user() -> [
	   {doc, "Open an ftp connection to a host, and logon as anonymous ftp,"
	    " then logoff"}].
user(Config) ->
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS("")),% logon
    ok = ftp:close(Pid),			% logoff
    {error,eclosed} = ftp:pwd(Pid),		% check logoff result
    ok.

%%-------------------------------------------------------------------------
bad_user() -> 
    [{doc, "Open an ftp connection to a host, and logon with bad user."}].
bad_user(Config) ->
    Pid = proplists:get_value(ftp, Config),
    {error, euser} = ftp:user(Pid, ?BAD_USER, ?FTP_PASS("")),
    ok.

%%-------------------------------------------------------------------------
pwd() -> 
    [{doc, "Test ftp:pwd/1 & ftp:lpwd/1"}].
pwd(Config0) ->
    Config = set_state([reset], Config0),
    Pid = proplists:get_value(ftp, Config),
    {ok, PWD} = ftp:pwd(Pid),
    {ok, PathLpwd} = ftp:lpwd(Pid),
    PWD = id2ftp_result("", Config),
    PathLpwd = id2ftp_result("", Config).

%%-------------------------------------------------------------------------
cd() -> 
    ["Open an ftp connection, log on as anonymous ftp, and cd to a"
     "directory and to a non-existent directory."].
cd(Config0) ->
    Dir = "test",
    Config = set_state([reset,{mkdir,Dir}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:cd(Pid, id2ftp(Dir,Config)),
    {ok, PWD} = ftp:pwd(Pid),
    ExpectedPWD = id2ftp_result(Dir, Config),
    PWD = ExpectedPWD,
    {error, epath} = ftp:cd(Pid, ?BAD_DIR),
    ok.

%%-------------------------------------------------------------------------
lcd() ->
    [{doc, "Test api function ftp:lcd/2"}].
lcd(Config0) ->
    Dir = "test",
    Config = set_state([reset,{mkdir,Dir}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:lcd(Pid, id2ftp(Dir,Config)),
    {ok, PWD} = ftp:lpwd(Pid),
    ExpectedPWD = id2ftp_result(Dir, Config),
    PWD = ExpectedPWD,
    {error, epath} = ftp:lcd(Pid, ?BAD_DIR).

%%-------------------------------------------------------------------------
ls() -> 
    [{doc, "Open an ftp connection; ls the current directory, and the "
      "\"test\" directory. We assume that ls never fails, since "
      "it's output is meant to be read by humans. "}].
ls(Config0) ->
    Config = set_state([reset,{mkdir,"test"}], Config0),
    Pid = proplists:get_value(ftp, Config),
    {ok, _R1} = ftp:ls(Pid),
    {ok, _R2} = ftp:ls(Pid, id2ftp("test",Config)),
    %% neither nlist nor ls operates on a directory
    %% they operate on a pathname, which *can* be a 
    %% directory, but can also be a filename or a group 
    %% of files (including wildcards).
    case proplists:get_value(wildcard_support, Config) of
	true ->
	    {ok, _R3} = ftp:ls(Pid, id2ftp("te*",Config));
	_ ->
	    ok
    end.

%%-------------------------------------------------------------------------
nlist() -> 
    [{doc,"Open an ftp connection; nlist the current directory, and the "
	       "\"test\" directory. Nlist does not behave consistenly over "
	       "operating systems. On some it is an error to have an empty "
	       "directory."}].
nlist(Config0) ->
    Config = set_state([reset,{mkdir,"test"}], Config0),
    Pid = proplists:get_value(ftp, Config),
    {ok, _R1} = ftp:nlist(Pid),
    {ok, _R2} = ftp:nlist(Pid, id2ftp("test",Config)),
    %% neither nlist nor ls operates on a directory
    %% they operate on a pathname, which *can* be a 
    %% directory, but can also be a filename or a group 
    %% of files (including wildcards).
    case proplists:get_value(wildcard_support, Config) of
	true ->
	    {ok, _R3} = ftp:nlist(Pid, id2ftp("te*",Config));
	_ ->
	    ok
    end.

%%-------------------------------------------------------------------------
rename() -> 
    [{doc, "Rename a file."}].
rename(Config0) ->
    Contents = <<"ftp_SUITE test ...">>,
    OldFile = "old.txt",
    NewFile = "new.txt",
    Config = set_state([reset,{mkfile,OldFile,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),

    ok = ftp:rename(Pid, 
		    id2ftp(OldFile,Config),
		    id2ftp(NewFile,Config)),

    true = (chk_file(NewFile,Contents,Config) 
	    and chk_no_file([OldFile],Config)),
    {error,epath} = ftp:rename(Pid,
			       id2ftp("non_existing_file",Config),
			       id2ftp(NewFile,Config)),
    ok.

%%-------------------------------------------------------------------------
send() -> 
    [{doc, "Transfer a file with ftp using send/2."}].
send(Config0) ->
    Contents = <<"ftp_SUITE test ...">>,
    SrcDir = "data",
    File = "file.txt",
    Config = set_state([reset,{mkfile,[SrcDir,File],Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),

    chk_no_file([File],Config),
    chk_file([SrcDir,File],Contents,Config),

    ok = ftp:lcd(Pid, id2ftp(SrcDir,Config)),
    ok = ftp:cd(Pid, id2ftp("",Config)),
    ok = ftp:send(Pid, File),
    chk_file(File, Contents, Config),

    {error,epath} = ftp:send(Pid, "non_existing_file"),
    ok.

%%-------------------------------------------------------------------------
send_3() -> 
    [{doc, "Transfer a file with ftp using send/3."}].
send_3(Config0) ->
    Contents = <<"ftp_SUITE test ...">>,
    Dir = "incoming",
    File = "file.txt",
    RemoteFile = "remfile.txt",
    Config = set_state([reset,{mkfile,File,Contents},{mkdir,Dir}], Config0),
    Pid = proplists:get_value(ftp, Config),

    ok = ftp:cd(Pid, id2ftp(Dir,Config)),
    ok = ftp:lcd(Pid, id2ftp("",Config)),
    ok = ftp:send(Pid, File, RemoteFile),
    chk_file([Dir,RemoteFile], Contents, Config),

    {error,epath} = ftp:send(Pid, "non_existing_file", RemoteFile),
    ok.

%%------------------------------------------------------------------------- 
send_bin() -> 
    [{doc, "Send a binary."}].
send_bin(Config0) ->
    BinContents = <<"ftp_SUITE test ...">>,
    File = "file.txt",
    Config = set_state([reset], Config0),
    Pid = proplists:get_value(ftp, Config),
    {error, enotbinary} = ftp:send_bin(Pid, "some string", id2ftp(File,Config)),
    ok = ftp:send_bin(Pid, BinContents, id2ftp(File,Config)),
    chk_file(File, BinContents, Config),
    {error, efnamena} = ftp:send_bin(Pid, BinContents, "/nothere"),
    ok.

%%-------------------------------------------------------------------------    
send_chunk() -> 
    [{doc, "Send a binary using chunks."}].
send_chunk(Config0) ->
    Contents1 = <<"1: ftp_SUITE test ...">>,
    Contents2 = <<"2: ftp_SUITE test ...">>,
    File = "file.txt",
    Config = set_state([reset,{mkdir,"incoming"}], Config0),
    Pid = proplists:get_value(ftp, Config),

    ok = ftp:send_chunk_start(Pid, id2ftp(File,Config)),
    {error, echunk} = ftp:send_chunk_start(Pid, id2ftp(File,Config)),
    {error, echunk} = ftp:cd(Pid, "incoming"),
    {error, enotbinary} = ftp:send_chunk(Pid, "some string"),
    ok = ftp:send_chunk(Pid, Contents1),
    ok = ftp:send_chunk(Pid, Contents2),
    ok = ftp:send_chunk_end(Pid),
    chk_file(File, <<Contents1/binary,Contents2/binary>>, Config),

    {error, echunk} = ftp:send_chunk(Pid, Contents1),
    {error, echunk} = ftp:send_chunk_end(Pid),
    {error, efnamena} = ftp:send_chunk_start(Pid, "/"),
    ok.

%%-------------------------------------------------------------------------
delete() -> 
    [{doc, "Delete a file."}].
delete(Config0) ->
    Contents = <<"ftp_SUITE test ...">>,
    File = "file.txt",
    Config = set_state([reset,{mkfile,File,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:delete(Pid, id2ftp(File,Config)),
    chk_no_file([File], Config),
    {error,epath} = ftp:delete(Pid, id2ftp(File,Config)),
    ok.

%%-------------------------------------------------------------------------
mkdir() ->
    [{doc, "Make a remote directory."}].
mkdir(Config0) ->
    NewDir = "new_dir",
    Config = set_state([reset], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:mkdir(Pid, id2ftp(NewDir,Config)),
    chk_dir([NewDir], Config),
    {error,epath} = ftp:mkdir(Pid, id2ftp(NewDir,Config)),
    ok.

%%-------------------------------------------------------------------------
rmdir() -> 
    [{doc, "Remove a directory."}].
rmdir(Config0) ->
    Dir = "dir",
    Config = set_state([reset,{mkdir,Dir}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:rmdir(Pid, id2ftp(Dir,Config)),
    chk_no_dir([Dir], Config),
    {error,epath} = ftp:rmdir(Pid, id2ftp(Dir,Config)),
    ok.

%%-------------------------------------------------------------------------
append() -> 
    [{doc, "Append a local file twice to a remote file"}].
append(Config0) ->
    SrcFile = "f_src.txt",
    DstFile = "f_dst.txt",
    Contents = <<"ftp_SUITE test ...">>,
    Config = set_state([reset,{mkfile,SrcFile,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:append(Pid, id2ftp(SrcFile,Config), id2ftp(DstFile,Config)),
    ok = ftp:append(Pid, id2ftp(SrcFile,Config), id2ftp(DstFile,Config)),
    chk_file(DstFile, <<Contents/binary,Contents/binary>>, Config),
    {error,epath} = ftp:append(Pid, id2ftp("non_existing_file",Config), id2ftp(DstFile,Config)),
    ok.
		
%%-------------------------------------------------------------------------
append_bin() -> 
    [{doc, "Append a local file twice to a remote file using append_bin"}].
append_bin(Config0) ->
    DstFile = "f_dst.txt",
    Contents = <<"ftp_SUITE test ...">>,
    Config = set_state([reset], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:append_bin(Pid, Contents, id2ftp(DstFile,Config)),
    ok = ftp:append_bin(Pid, Contents, id2ftp(DstFile,Config)),
    chk_file(DstFile, <<Contents/binary,Contents/binary>>, Config).

%%-------------------------------------------------------------------------
append_chunk() -> 
    [{doc, "Append chunks."}].
append_chunk(Config0) ->
    File = "f_dst.txt",
    Contents = [<<"ER">>,<<"LE">>,<<"RL">>],
    Config = set_state([reset], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:append_chunk_start(Pid, id2ftp(File,Config)),
    {error, enotbinary} = ftp:append_chunk(Pid, binary_to_list(lists:nth(1,Contents))),
    ok = ftp:append_chunk(Pid,lists:nth(1,Contents)),
    ok = ftp:append_chunk(Pid,lists:nth(2,Contents)),
    ok = ftp:append_chunk(Pid,lists:nth(3,Contents)),
    ok = ftp:append_chunk_end(Pid),
    chk_file(File, <<"ERLERL">>, Config).

%%-------------------------------------------------------------------------
recv() -> 
    [{doc, "Receive a file using recv/2"}].
recv(Config0) ->
    File1 = "f_dst1.txt",
    File2 = "f_dst2.txt",
    SrcDir = "a_dir",
    Contents1 = <<"1 ftp_SUITE test ...">>,
    Contents2 = <<"2 ftp_SUITE test ...">>,
    Config = set_state([reset, {mkfile,[SrcDir,File1],Contents1}, {mkfile,[SrcDir,File2],Contents2}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:cd(Pid, id2ftp(SrcDir,Config)),
    ok = ftp:lcd(Pid, id2ftp("",Config)),
    ok = ftp:recv(Pid, File1),
    chk_file(File1, Contents1, Config),
    ok = ftp:recv(Pid, File2),
    chk_file(File2, Contents2, Config),
    {error,epath} = ftp:recv(Pid, "non_existing_file"),
    ok.

%%-------------------------------------------------------------------------
recv_3() -> 
    [{doc,"Receive a file using recv/3"}].
recv_3(Config0) ->
    DstFile = "f_src.txt",
    SrcFile = "f_dst.txt",
    Contents = <<"ftp_SUITE test ...">>,
    Config = set_state([reset, {mkfile,SrcFile,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:cd(Pid, id2ftp("",Config)),
    ok = ftp:recv(Pid, SrcFile, id2abs(DstFile,Config)),
    chk_file(DstFile, Contents, Config).

%%-------------------------------------------------------------------------
recv_bin() -> 
    [{doc, "Receive a file as a binary."}].
recv_bin(Config0) ->
    File = "f_dst.txt",
    Contents = <<"ftp_SUITE test ...">>,
    Config = set_state([reset, {mkfile,File,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    {ok,Received} = ftp:recv_bin(Pid, id2ftp(File,Config)),
    find_diff(Received, Contents),
    {error,epath} = ftp:recv_bin(Pid, id2ftp("non_existing_file",Config)),
    ok.

%%-------------------------------------------------------------------------
recv_bin_twice() -> 
    [{doc, "Receive two files as a binaries."}].
recv_bin_twice(Config0) ->
    File1 = "f_dst1.txt",
    File2 = "f_dst2.txt",
    Contents1 = <<"1 ftp_SUITE test ...">>,
    Contents2 = <<"2 ftp_SUITE test ...">>,
    Config = set_state([reset, {mkfile,File1,Contents1}, {mkfile,File2,Contents2}], Config0),
    ct:log("First transfer",[]),
    Pid = proplists:get_value(ftp, Config),
    {ok,Received1} = ftp:recv_bin(Pid, id2ftp(File1,Config)),
    find_diff(Received1, Contents1),
    ct:log("Second transfer",[]),
    {ok,Received2} = ftp:recv_bin(Pid, id2ftp(File2,Config)),
    find_diff(Received2, Contents2),
    ct:log("Transfers ready!",[]),
    {error,epath} = ftp:recv_bin(Pid, id2ftp("non_existing_file",Config)),
    ok.
%%-------------------------------------------------------------------------
recv_chunk() -> 
    [{doc, "Receive a file using chunk-wise."}].
recv_chunk(Config0) ->
    File = "big_file.txt",
    Contents = list_to_binary( lists:duplicate(1000, lists:seq(0,255)) ),
    Config = set_state([reset, {mkfile,File,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    {error, "ftp:recv_chunk_start/2 not called"} = do_recv_chunk(Pid),
    ok = ftp:recv_chunk_start(Pid, id2ftp(File,Config)),
    {ok, ReceivedContents} = do_recv_chunk(Pid),
    find_diff(ReceivedContents, Contents).

recv_chunk_twice() ->
    [{doc, "Receive two files using chunk-wise."}].
recv_chunk_twice(Config0) ->
    File1 = "big_file1.txt",
    File2 = "big_file2.txt",
    Contents1 = list_to_binary( lists:duplicate(1000, lists:seq(0,255)) ),
    Contents2 = crypto:strong_rand_bytes(1200),
    Config = set_state([reset, {mkfile,File1,Contents1}, {mkfile,File2,Contents2}], Config0),
    Pid = proplists:get_value(ftp, Config),
    {error, "ftp:recv_chunk_start/2 not called"} = do_recv_chunk(Pid),
    ok = ftp:recv_chunk_start(Pid, id2ftp(File1,Config)),
    {ok, ReceivedContents1} = do_recv_chunk(Pid),
    ok = ftp:recv_chunk_start(Pid, id2ftp(File2,Config)),
    {ok, ReceivedContents2} = do_recv_chunk(Pid),
    find_diff(ReceivedContents1, Contents1),
    find_diff(ReceivedContents2, Contents2).

recv_chunk_three_times() ->
    [{doc, "Receive two files using chunk-wise."},
     {timetrap,{seconds,120}}].
recv_chunk_three_times(Config0) ->
    File1 = "big_file1.txt",
    File2 = "big_file2.txt",
    File3 = "big_file3.txt",
    Contents1 = list_to_binary( lists:duplicate(1000, lists:seq(0,255)) ),
    Contents2 = crypto:strong_rand_bytes(1200),
    Contents3 = list_to_binary( lists:duplicate(1000, lists:seq(255,0,-1)) ),

    Config = set_state([reset, {mkfile,File1,Contents1}, {mkfile,File2,Contents2}, {mkfile,File3,Contents3}], Config0),
    Pid = proplists:get_value(ftp, Config),
    {error, "ftp:recv_chunk_start/2 not called"} = do_recv_chunk(Pid),

    ok = ftp:recv_chunk_start(Pid, id2ftp(File3,Config)),
    {ok, ReceivedContents3} = do_recv_chunk(Pid),
    
    ok = ftp:recv_chunk_start(Pid, id2ftp(File1,Config)),
    {ok, ReceivedContents1} = do_recv_chunk(Pid),

    ok = ftp:recv_chunk_start(Pid, id2ftp(File2,Config)),
    {ok, ReceivedContents2} = do_recv_chunk(Pid),

    find_diff(ReceivedContents1, Contents1),
    find_diff(ReceivedContents2, Contents2),
    find_diff(ReceivedContents3, Contents3).


do_recv_chunk(Pid) -> 
    recv_chunk(Pid, <<>>).
recv_chunk(Pid, Acc) -> 
    case ftp:recv_chunk(Pid) of
	ok -> 
            {ok, Acc};
	{ok, Bin} -> 
            recv_chunk(Pid, <<Acc/binary, Bin/binary>>);
	Error -> 
            Error
    end.

recv_chunk_delay(Config0) when is_list(Config0) ->
    File1 = "big_file1.txt",
    Contents = list_to_binary(lists:duplicate(1000, lists:seq(0,255))),
    Config = set_state([reset, {mkfile,File1,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:recv_chunk_start(Pid, id2ftp(File1,Config)),
    {ok, ReceivedContents} = delay_recv_chunk(Pid),
    find_diff(ReceivedContents, Contents).

delay_recv_chunk(Pid) -> 
     delay_recv_chunk(Pid, <<>>).
delay_recv_chunk(Pid, Acc) -> 
    ct:pal("Recived size ~p", [byte_size(Acc)]),
    case ftp:recv_chunk(Pid) of
 	ok -> 
             {ok, Acc};
 	{ok, Bin} -> 
            ct:sleep(100),
            delay_recv_chunk(Pid, <<Acc/binary, Bin/binary>>);
	Error -> 
            Error
     end.

%%-------------------------------------------------------------------------
type() -> 
    [{doc,"Test that we can change btween ASCCI and binary transfer mode"}].
type(Config) ->
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:type(Pid, ascii),
    ok = ftp:type(Pid, binary),
    ok = ftp:type(Pid, ascii),
    {error, etype} = ftp:type(Pid, foobar).

%%-------------------------------------------------------------------------
quote(Config) ->
    Pid = proplists:get_value(ftp, Config),
    ["257 \""++_Rest] = ftp:quote(Pid, "pwd"), %% 257
    [_| _] = ftp:quote(Pid, "help"),
    %% This negativ test causes some ftp servers to hang. This test
    %% is not important for the client, so we skip it for now.
    %%["425 Can't build data connection: Connection refused."] 
    %% = ftp:quote(Pid, "list"), 
    ok.

%%-------------------------------------------------------------------------
progress_report_send() ->
    [{doc, "Test the option progress for ftp:send/[2,3]"}].
progress_report_send(Config) when is_list(Config) ->
    ReportPid = 
	spawn_link(?MODULE, progress_report_receiver_init, [self(), 1]),
    send(Config),
    receive
	{ReportPid, ok} ->
	    ok
    end.

%%-------------------------------------------------------------------------
progress_report_recv() ->
    [{doc, "Test the option progress for ftp:recv/[2,3]"}].
progress_report_recv(Config) when is_list(Config) ->
    ReportPid = 
 	spawn_link(?MODULE, progress_report_receiver_init, [self(), 3]),
    recv(Config),
    receive
 	{ReportPid, ok} ->
 	    ok
    end.

%%-------------------------------------------------------------------------

not_owner() ->
    [{doc, "Test what happens if a process that not owns the connection tries "
    "to use it"}].
not_owner(Config) when is_list(Config) ->
    Pid = proplists:get_value(ftp, Config),

    Parent = self(),
    OtherPid = spawn_link(
		 fun() ->
			 {error, not_connection_owner} = ftp:pwd(Pid),
			 ftp:close(Pid),
			 Parent ! {self(), ok}
		 end),
    receive
	{OtherPid, ok} ->
	    {ok, _} = ftp:pwd(Pid)
    end.


%%-------------------------------------------------------------------------


unexpected_call()->
    [{doc, "Test that behaviour of the ftp process if the api is abused"}].
unexpected_call(Config) when is_list(Config) ->
    Flag =  process_flag(trap_exit, true),
    Pid = proplists:get_value(ftp, Config),
    
    %% Serious programming fault, connetion will be shut down 
    case (catch gen_server:call(Pid, {self(), foobar, 10}, infinity)) of
	{error, {connection_terminated, 'API_violation'}} ->
	    ok;
	Unexpected1 ->
	    exit({unexpected_result, Unexpected1})
    end,
    ct:sleep(500),
    undefined = process_info(Pid, status),
    process_flag(trap_exit, Flag).
%%-------------------------------------------------------------------------

unexpected_cast()->
    [{doc, "Test that behaviour of the ftp process if the api is abused"}].
unexpected_cast(Config) when is_list(Config) ->
    Flag = process_flag(trap_exit, true),
    Pid = proplists:get_value(ftp, Config),
    %% Serious programming fault, connetion will be shut down 
    gen_server:cast(Pid, {self(), foobar, 10}),
    ct:sleep(500),
    undefined = process_info(Pid, status),
    process_flag(trap_exit, Flag).
%%-------------------------------------------------------------------------
 
unexpected_bang()->
    [{doc, "Test that connection ignores unexpected bang"}].
unexpected_bang(Config) when is_list(Config) ->
    Flag = process_flag(trap_exit, true),
    Pid = proplists:get_value(ftp, Config),
    %% Could be an innocent misstake the connection lives. 
    Pid ! foobar, 
    ct:sleep(500),
    {status, _} = process_info(Pid, status),
    process_flag(trap_exit, Flag).
    
%%-------------------------------------------------------------------------

clean_shutdown() -> 
    [{doc, "Test that owning process that exits with reason "
     "'shutdown' does not cause an error message. OTP 6035"}].

clean_shutdown(Config) ->
    Parent = self(),
    HelperPid = spawn(
		  fun() ->
			  ftp__open(Config, [verbose]),
			  Parent ! ok,
			  receive
			      nothing -> ok
			  end
		  end),
    receive
	ok ->
	    PrivDir = proplists:get_value(priv_dir, Config),
	    LogFile = filename:join([PrivDir,"ticket_6035.log"]),
 	    error_logger:logfile({open, LogFile}),
	    exit(HelperPid, shutdown),
	    timer:sleep(2000),
	    error_logger:logfile(close),
	    case is_error_report_6035(LogFile) of
		true ->  ok;
		false -> {fail, "Bad logfile"}
	    end
    end.

%%-------------------------------------------------------------------------
start_ftp() ->
    [{doc, "Start/stop of ftp service"}].
start_ftp(Config) ->
    Pid0 = proplists:get_value(ftp,Config),
    Pids0 = [ServicePid || {_, ServicePid} <- ftp:services()],
    true = lists:member(Pid0, Pids0),
    {ok, [_|_]} = ftp:service_info(Pid0),
    ftp:stop_service(Pid0),
    ct:sleep(100),
    Pids1 =  [ServicePid || {_, ServicePid} <- ftp:services()], 
    false = lists:member(Pid0, Pids1),

    Host = proplists:get_value(ftpd_host,Config),
    Port = proplists:get_value(ftpd_port,Config),

    {ok, Pid1} = ftp:start_standalone([{host, Host},{port, Port}]),
    Pids2 =  [ServicePid || {_, ServicePid} <- ftp:services()],
    false = lists:member(Pid1, Pids2).

%%-------------------------------------------------------------------------
ftp_worker() ->
    [{doc, "Makes sure the ftp worker processes are added and removed "
      "appropriatly to/from the supervison tree."}].
ftp_worker(Config) ->
    Pid = proplists:get_value(ftp,Config),
    case supervisor:which_children(ftp_sup) of
        [{_,_, worker, [ftp]}] ->
            ftp:stop_service(Pid), 
            ct:sleep(5000),
            [] = supervisor:which_children(ftp_sup),
            ok;
        Children ->
            ct:fail("Unexpected children: ~p",[Children])
    end.


%%%----------------------------------------------------------------
%%% Error codes not tested elsewhere

error_elogin(Config0) ->
    Dir = "test",
    OldFile = "old.txt",
    NewFile = "new.txt",
    SrcDir = "data",
    File = "file.txt",
    Config = set_state([reset,
			{mkdir,Dir},
			{mkfile,OldFile,<<"Contents..">>},
			{mkfile,[SrcDir,File],<<"Contents..">>}], Config0),

    Pid = proplists:get_value(ftp, Config),
    ok = ftp:lcd(Pid, id2ftp(SrcDir,Config)),
    {error,elogin} = ftp:send(Pid, File),
    ok = ftp:lcd(Pid, id2ftp("",Config)),
    {error,elogin} = ftp:pwd(Pid),
    {error,elogin} = ftp:cd(Pid, id2ftp(Dir,Config)),
    {error,elogin} = ftp:rename(Pid, 
				id2ftp(OldFile,Config),
				id2ftp(NewFile,Config)),
    ok.

error_ehost(_Config) ->
    {error, ehost} = ftp:open("nohost.nodomain"),
    ok.
    
%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------

chk_file(Path=[C|_], ExpectedContents, Config) when 0<C,C=<255 ->
    chk_file([Path], ExpectedContents, Config);

chk_file(PathList, ExpectedContents, Config) ->
    Path = filename:join(PathList),
    AbsPath = id2abs(Path,Config),
    case file:read_file(AbsPath) of
	{ok,ExpectedContents} -> 
	    true;
	{ok,ReadContents} -> 
	    {error,{diff,Pos,RC,LC}} = find_diff(ReadContents, ExpectedContents, 1),
	    ct:log("Bad contents of ~p.~nGot:~n~p~nExpected:~n~p~nDiff at pos ~p ~nRead: ~p~nExp : ~p",
		   [AbsPath,ReadContents,ExpectedContents,Pos,RC,LC]),
	    ct:fail("Bad contents of ~p", [Path]);
	{error,Error} ->
	    try begin
		    {ok,CWD} = file:get_cwd(),
		    ct:log("file:get_cwd()=~p~nfiles:~n~p",[CWD,file:list_dir(CWD)])
		end
	    of _ -> ok
	    catch _:_ ->ok
	    end,
	    ct:fail("Error reading ~p: ~p",[Path,Error])
    end.


chk_no_file(Path=[C|_], Config) when 0<C,C=<255 ->
    chk_no_file([Path], Config);

chk_no_file(PathList, Config) ->
    Path = filename:join(PathList),
    AbsPath = id2abs(Path,Config),
    case file:read_file(AbsPath) of
	{error,enoent} -> 
	    true;
	{ok,Contents} -> 
	    ct:log("File ~p exists although it shouldn't. Contents:~n~p",
		   [AbsPath,Contents]),
	    ct:fail("File exists: ~p", [Path]);
	{error,Error} ->
	    ct:fail("Unexpected error reading ~p: ~p",[Path,Error])
    end.


chk_dir(Path=[C|_], Config) when 0<C,C=<255 ->
    chk_dir([Path], Config);

chk_dir(PathList, Config) ->
    Path = filename:join(PathList),
    AbsPath = id2abs(Path,Config),
    case file:read_file_info(AbsPath) of
	{ok, #file_info{type=directory}} ->
	    true;
	{ok, #file_info{type=Type}} ->
	    ct:fail("Expected dir ~p is a ~p",[Path,Type]);
	{error,Error} ->
	    ct:fail("Expected dir ~p: ~p",[Path,Error])
    end.

chk_no_dir(PathList, Config) ->
    Path = filename:join(PathList),
    AbsPath = id2abs(Path,Config),
    case file:read_file_info(AbsPath) of
	{error,enoent} ->
	    true;
	{ok, #file_info{type=directory}} ->
	    ct:fail("Dir ~p erroneously exists",[Path]);
	{ok, #file_info{type=Type}} ->
	    ct:fail("~p ~p erroneously exists",[Type,Path]);
	{error,Error} ->
	    ct:fail("Unexpected error for ~p: ~p",[Path,Error])
    end.

%%--------------------------------------------------------------------
find_executable(Config) ->
    search_executable(proplists:get_value(ftpservers, Config, ?default_ftp_servers)).


search_executable([{Name,Paths,_StartCmd,_ChkUp,_StopCommand,_ConfigUpd,_Host,_Port}|Srvrs]) ->
    case os_find(Name,Paths) of
	false ->
	    ct:log("~p not found",[Name]),
	    search_executable(Srvrs);
	AbsName -> 
	    ct:comment("Found ~p",[AbsName]),
	    {ok, {AbsName,_StartCmd,_ChkUp,_StopCommand,_ConfigUpd,_Host,_Port}}
    end;
search_executable([]) ->
    false.


os_find(Name, Paths) ->
    case os:find_executable(Name, Paths) of
	false -> os:find_executable(Name);
	AbsName -> AbsName
    end.

%%%----------------------------------------------------------------
start_ftpd(Config0) ->
    {AbsName,StartCmd,_ChkUp,_StopCommand,ConfigRewrite,Host,Port} =
	proplists:get_value(ftpd_data, Config0),
    case StartCmd(Config0, AbsName) of
	{ok,StartResult} ->
	    Config = [{ftpd_host,Host},
		      {ftpd_port,Port},
		      {ftpd_start_result,StartResult} | ConfigRewrite(Config0)],
	    try
		ftp__close(ftp__open(Config,[verbose]))
	    of
		Config1 when is_list(Config1) ->
		    ct:log("Usuable ftp server ~p started on ~p:~p",[AbsName,Host,Port]),
		    Config
	    catch
		Class:Exception ->
		    ct:log("Ftp server ~p started on ~p:~p but is unusable:~n~p:~p",
			   [AbsName,Host,Port,Class,Exception]),
		    {skip, [AbsName," started but unusuable"]}
	    end;
	{error,Msg} ->
	    {skip, [AbsName," not started: ",Msg]}
    end.

stop_ftpd(Config) ->
    {_Name,_StartCmd,_ChkUp,StopCommand,_ConfigUpd,_Host,_Port} = proplists:get_value(ftpd_data, Config),
    StopCommand(proplists:get_value(ftpd_start_result,Config)).

ps_ftpd(Config) ->
    {_Name,_StartCmd,ChkUp,_StopCommand,_ConfigUpd,_Host,_Port} = proplists:get_value(ftpd_data, Config),
    ct:log( ChkUp(proplists:get_value(ftpd_start_result,Config)) ).


ftpd_running(Config) ->
    {_Name,_StartCmd,ChkUp,_StopCommand,_ConfigUpd,_Host,_Port} = proplists:get_value(ftpd_data, Config),
    ChkUp(proplists:get_value(ftpd_start_result,Config)).

ftp__open(Config, Options) ->
    Host = proplists:get_value(ftpd_host,Config),
    Port = proplists:get_value(ftpd_port,Config),
    ct:log("Host=~p, Port=~p",[Host,Port]),
    {ok,Pid} = ftp:open(Host, [{port,Port} | Options]),
    [{ftp,Pid}|Config].

ftp__close(Config) ->
    ok = ftp:close(proplists:get_value(ftp,Config)),
    Config.

ftp_start_service(Config, Options) ->
    Host = proplists:get_value(ftpd_host,Config),
    Port = proplists:get_value(ftpd_port,Config),
    ct:log("Host=~p, Port=~p",[Host,Port]),
    {ok,Pid} = ftp:start_service([{host, Host},{port,Port} | Options]),
    [{ftp,Pid}|Config].

ftp_stop_service(Config) ->
    ok = ftp:stop_service(proplists:get_value(ftp,Config)),
    Config.

split(Cs) -> string:tokens(Cs, "\r\n").

find_diff(Bin1, Bin2) -> 
    case find_diff(Bin1, Bin2, 1) of
	{error, {diff,Pos,RC,LC}} ->
	    ct:log("Contents differ at position ~p.~nOp1: ~p~nOp2: ~p",[Pos,RC,LC]),
	    ct:fail("Contents differ at pos ~p",[Pos]);
	Other ->
	    Other
    end.

find_diff(A, A, _) -> true;
find_diff(<<H,T1/binary>>, <<H,T2/binary>>, Pos) -> find_diff(T1, T2, Pos+1);
find_diff(RC, LC, Pos) -> {error, {diff, Pos, RC, LC}}.

set_state(Ops, Config) when is_list(Ops) -> lists:foldl(fun set_state/2, Config, Ops);

set_state(reset, Config) -> 
    rm('*', id2abs("",Config)),
    PrivDir = proplists:get_value(priv_dir,Config),
    file:set_cwd(PrivDir),
    ftp:lcd(proplists:get_value(ftp,Config),PrivDir),
    set_state({mkdir,""},Config);
set_state({mkdir,Id}, Config) ->
    Abs = id2abs(Id, Config),
    mk_path(Abs),
    file:make_dir(Abs),
    Config;
set_state({mkfile,Id,Contents}, Config) ->
    Abs = id2abs(Id, Config),
    mk_path(Abs),
    ok = file:write_file(Abs, Contents),
    Config.

mk_path(Abs) -> lists:foldl(fun mk_path/2, [], filename:split(filename:dirname(Abs))).

mk_path(F, Pfx) ->
    case file:read_file_info(AbsName=filename:join(Pfx,F)) of
	{ok,#file_info{type=directory}} ->
	    AbsName;
	{error,eexist} ->
	    AbsName;
	{error,enoent} ->
	    ok = file:make_dir(AbsName),
	    AbsName
    end.
    
rm('*', Pfx) ->
    {ok,Fs} = file:list_dir(Pfx),
    lists:foreach(fun(F) -> rm(F, Pfx) end, Fs);
rm(F, Pfx) -> 
    case file:read_file_info(AbsName=filename:join(Pfx,F)) of
	{ok,#file_info{type=directory}} ->
	    {ok,Fs} = file:list_dir(AbsName),
	    lists:foreach(fun(F1) -> rm(F1,AbsName) end, Fs),
	    ok = file:del_dir(AbsName);

	{ok,#file_info{type=regular}} ->
	    ok = file:delete(AbsName);

	{error,enoent} ->
	    ok
    end.

id2abs(Id, Conf) -> filename:join(proplists:get_value(priv_dir,Conf),ids(Id)).
id2ftp(Id, Conf) -> (proplists:get_value(id2ftp,Conf))(ids(Id)).
id2ftp_result(Id, Conf) -> (proplists:get_value(id2ftp_result,Conf))(ids(Id)).

ids([[_|_]|_]=Ids) -> filename:join(Ids);
ids(Id) -> Id.


is_expected_absName(Id, File, Conf) -> File = (proplists:get_value(id2abs,Conf))(Id).
is_expected_ftpInName(Id, File, Conf) -> File = (proplists:get_value(id2ftp,Conf))(Id).
is_expected_ftpOutName(Id, File, Conf) -> File = (proplists:get_value(id2ftp_result,Conf))(Id).


%%%----------------------------------------------------------------
%%% Help functions for the option '{progress,Progress}'
%%%

%%%----------------
%%% Callback:

progress(#progress{} = P, _File, {file_size, Total} = M) ->
    ct:pal("Progress: ~p",[M]),
    progress_report_receiver ! start,
    P#progress{total = Total};

progress(#progress{current = Current} = P, _File, {transfer_size, 0} = M) ->
    ct:pal("Progress: ~p",[M]),
    progress_report_receiver ! finish,
    case P#progress.total of
	unknown -> P;
	Current -> P;
	Total   -> ct:fail({error, {progress, {total,Total}, {current,Current}}}),
		   P
    end;

progress(#progress{current = Current} = P, _File, {transfer_size, Size} = M) ->
    ct:pal("Progress: ~p",[M]),
    progress_report_receiver ! update,
    P#progress{current = Current + Size};

progress(P, _File, M) ->
    ct:pal("Progress **** Strange: ~p",[M]),
    P.


%%%----------------
%%% Help process that counts the files transferred:

progress_report_receiver_init(Parent, N) ->
    register(progress_report_receiver, self()),
    progress_report_receiver_expect_N_files(Parent, N).

progress_report_receiver_expect_N_files(_Parent, 0) ->
    ct:pal("progress_report got all files!", []);
progress_report_receiver_expect_N_files(Parent, N) ->
    ct:pal("progress_report expects ~p more files",[N]),
    receive
	start -> ok
    end,
    progress_report_receiver_loop(Parent, N-1).


progress_report_receiver_loop(Parent, N) ->
    ct:pal("progress_report expect update | finish. N = ~p",[N]),
    receive
	update ->  
	    ct:pal("progress_report got update",[]),
	    progress_report_receiver_loop(Parent, N);
	finish  -> 
	    ct:pal("progress_report got finish, send ~p to ~p",[{self(),ok}, Parent]),
	    Parent ! {self(), ok},
	    progress_report_receiver_expect_N_files(Parent, N)
    end.

%%%----------------------------------------------------------------
%%% Help functions for bug OTP-6035

is_error_report_6035(LogFile) ->
    case file:read_file(LogFile) of
	{ok, Bin} -> 
	    nomatch =/= binary:match(Bin, <<"=ERROR REPORT====">>);
	_ -> 
	    false
    end.

