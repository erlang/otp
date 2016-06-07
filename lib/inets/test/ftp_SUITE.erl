%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%% 
%% ct:run("../inets_test", ftp_SUITE).
%%

-module(ftp_SUITE).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

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
all() ->
    [
     {group, ftp_passive},
     {group, ftp_active},
     {group, ftps_passive},
     {group, ftps_active}
    ].

groups() ->
    [
     {ftp_passive, [], ftp_tests()},
     {ftp_active, [], ftp_tests()},
     {ftps_passive, [], ftp_tests()},
     {ftps_active, [], ftp_tests()}
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
     recv_chunk, 
     type, 
     quote, 
     ip_v6_disabled,
     progress_report_send,
     progress_report_recv,
     not_owner,
     unexpected_call,
     unexpected_cast,
     unexpected_bang,
     clean_shutdown
    ].

%%--------------------------------------------------------------------

%%% Config
%%% key			meaning
%%% ................................................................
%%% ftpservers		list of servers to check if they are available
%%%			The element is:
%%%			  {Name,         % string(). The os command name
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
	  fun(__CONF__) -> 
		  DataDir = proplists:get_value(data_dir,__CONF__),
		  ConfFile = filename:join(DataDir, "vsftpd.conf"),
		  PrivDir = proplists:get_value(priv_dir,__CONF__),
		  AnonRoot = PrivDir,
		  Cmd = ["vsftpd "++filename:join(DataDir,"vsftpd.conf"),
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
	    make_cert_files(dsa, rsa, "server-", proplists:get_value(data_dir,Config)),
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
init_per_group(_Group, Config) -> Config.
    
end_per_group(_Group, Config) -> Config.

%%--------------------------------------------------------------------

init_per_testcase(Case, Config)  when (Case =:= progress_report_send) orelse 
				      (Case =:= progress_report_recv) ->
    common_init_per_testcase(Case, [{progress, {?MODULE, progress, #progress{}}} | Config]);

init_per_testcase(Case, Config) ->
    common_init_per_testcase(Case, Config).

common_init_per_testcase(Case, Config0) ->
    Group = proplists:get_value(name,proplists:get_value(tc_group_properties,Config0)),
    try ?MODULE:Case(doc) of
	Msg -> ct:comment(Msg)
    catch
	_:_-> ok
    end,
    TLS = [{tls,[{reuse_sessions,true}]}],
    ACTIVE = [{mode,active}],
    PASSIVE = [{mode,passive}],
    ExtraOpts = [verbose],
    Config =
	case Group of
	    ftp_active   -> ftp__open(Config0,       ACTIVE  ++ExtraOpts);
	    ftps_active  -> ftp__open(Config0, TLS++ ACTIVE  ++ExtraOpts);
	    ftp_passive  -> ftp__open(Config0,      PASSIVE  ++ExtraOpts);
	    ftps_passive -> ftp__open(Config0, TLS++PASSIVE  ++ExtraOpts)
	end,
    case Case of
	user -> Config;
	bad_user -> Config;
	_ ->
	    Pid = proplists:get_value(ftp,Config),
	    ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS(atom_to_list(Group)++"-"++atom_to_list(Case)) ),
	    ok = ftp:cd(Pid, proplists:get_value(priv_dir,Config)),
	    Config
    end.

    
end_per_testcase(user, _Config) -> ok;
end_per_testcase(bad_user, _Config) -> ok;
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
    ftp__close(Config).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
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
    {error, epath} = ftp:cd(Pid, ?BAD_DIR).

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
	    and chk_no_file([OldFile],Config)).


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

    chk_file(File, Contents, Config).

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

    chk_file([Dir,RemoteFile], Contents, Config).

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
    chk_file(File, BinContents, Config).

%%-------------------------------------------------------------------------    
send_chunk() -> 
    [{doc, "Send a binary using chunks."}].
send_chunk(Config0) ->
    Contents = <<"ftp_SUITE test ...">>,
    File = "file.txt",
    Config = set_state([reset,{mkdir,"incoming"}], Config0),
    Pid = proplists:get_value(ftp, Config),

    ok = ftp:send_chunk_start(Pid, id2ftp(File,Config)),
    {error, echunk} = ftp:cd(Pid, "incoming"),
    {error, enotbinary} = ftp:send_chunk(Pid, "some string"),
    ok = ftp:send_chunk(Pid, Contents),
    ok = ftp:send_chunk(Pid, Contents),
    ok = ftp:send_chunk_end(Pid),
    chk_file(File, <<Contents/binary,Contents/binary>>, Config).

%%-------------------------------------------------------------------------
delete() -> 
    [{doc, "Delete a file."}].
delete(Config0) ->
    Contents = <<"ftp_SUITE test ...">>,
    File = "file.txt",
    Config = set_state([reset,{mkfile,File,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:delete(Pid, id2ftp(File,Config)),
    chk_no_file([File], Config).

%%-------------------------------------------------------------------------
mkdir() ->
    [{doc, "Make a remote directory."}].
mkdir(Config0) ->
    NewDir = "new_dir",
    Config = set_state([reset], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:mkdir(Pid, id2ftp(NewDir,Config)),
    chk_dir([NewDir], Config).

%%-------------------------------------------------------------------------
rmdir() -> 
    [{doc, "Remove a directory."}].
rmdir(Config0) ->
    Dir = "dir",
    Config = set_state([reset,{mkdir,Dir}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:rmdir(Pid, id2ftp(Dir,Config)),
    chk_no_dir([Dir], Config).

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
    chk_file(DstFile, <<Contents/binary,Contents/binary>>, Config).
		
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
    File = "f_dst.txt",
    SrcDir = "a_dir",
    Contents = <<"ftp_SUITE test ...">>,
    Config = set_state([reset, {mkfile,[SrcDir,File],Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    ok = ftp:cd(Pid, id2ftp(SrcDir,Config)),
    ok = ftp:lcd(Pid, id2ftp("",Config)),
    ok = ftp:recv(Pid, File),
    chk_file(File, Contents, Config).

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
    find_diff(Received, Contents).

%%-------------------------------------------------------------------------
recv_chunk() -> 
    [{doc, "Receive a file using chunk-wise."}].
recv_chunk(Config0) ->
    File = "big_file.txt",
    Contents = list_to_binary( lists:duplicate(1000, lists:seq(0,255)) ),
    Config = set_state([reset, {mkfile,File,Contents}], Config0),
    Pid = proplists:get_value(ftp, Config),
    {{error, "ftp:recv_chunk_start/2 not called"},_} = recv_chunk(Pid, <<>>),
    ok = ftp:recv_chunk_start(Pid, id2ftp(File,Config)),
    {ok, ReceivedContents, _Ncunks} = recv_chunk(Pid, <<>>),
    find_diff(ReceivedContents, Contents).

recv_chunk(Pid, Acc) -> 
    recv_chunk(Pid, Acc, 0).

recv_chunk(Pid, Acc, N) ->
    case ftp:recv_chunk(Pid) of
	ok -> {ok, Acc, N};
	{ok, Bin} -> recv_chunk(Pid, <<Acc/binary, Bin/binary>>, N+1);
	Error -> {Error, N}
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

%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------

make_cert_files(Alg1, Alg2, Prefix, Dir) ->
    CaInfo = {CaCert,_} = erl_make_certs:make_cert([{key,Alg1}]),
    {Cert,CertKey} = erl_make_certs:make_cert([{key,Alg2},{issuer,CaInfo}]),
    CaCertFile = filename:join(Dir, Prefix++"cacerts.pem"),
    CertFile = filename:join(Dir, Prefix++"cert.pem"),
    KeyFile = filename:join(Dir, Prefix++"key.pem"),
    der_to_pem(CaCertFile, [{'Certificate', CaCert, not_encrypted}]),
    der_to_pem(CertFile, [{'Certificate', Cert, not_encrypted}]),
    der_to_pem(KeyFile, [CertKey]),
    ok.

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).

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
    OtherPid = spawn_link(?MODULE, not_owner, [Pid, self()]),
    
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
    PrivDir = proplists:get_value(priv_dir, Config),
    LogFile = filename:join([PrivDir,"ticket_6035.log"]),
    Host = proplists:get_value(ftpd_host,Config),
    try
	Pid  = spawn(?MODULE, open_wait_6035, [Host, self()]),
	    error_logger:logfile({open, LogFile}),
	    true = kill_ftp_proc_6035(Pid, LogFile),
	    error_logger:logfile(close)
    catch 
	throw:{error, not_found} ->
	    {skip, "No available FTP servers"}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

find_executable(Config) ->
    FTPservers = case proplists:get_value(ftpservers,Config) of
		     undefined -> ?default_ftp_servers;
		     L -> L
		 end,
    case lists:dropwhile(fun not_available/1, FTPservers) of
	[] -> false;
	[FTPD_data|_] -> {ok, FTPD_data}
    end.

not_available({Name,_StartCmd,_ChkUp,_StopCommand,_ConfigUpd,_Host,_Port}) ->
    os:find_executable(Name) == false.


start_ftpd(Config) ->
    {Name,StartCmd,_ChkUp,_StopCommand,ConfigRewrite,Host,Port} = proplists:get_value(ftpd_data, Config),
    case StartCmd(Config) of
	{ok,StartResult} ->
	    [{ftpd_host,Host},
	     {ftpd_port,Port},
	     {ftpd_start_result,StartResult} | ConfigRewrite(Config)];
	{error,Msg} ->
	    {skip, [Name," not started: ",Msg]}
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

not_owner(FtpPid, Pid) ->
    {error, not_connection_owner} = ftp:pwd(FtpPid),
    ftp:close(FtpPid),
    ct:sleep(100),
    Pid ! {self(), ok}.

id2abs(Id, Conf) -> filename:join(proplists:get_value(priv_dir,Conf),ids(Id)).
id2ftp(Id, Conf) -> (proplists:get_value(id2ftp,Conf))(ids(Id)).
id2ftp_result(Id, Conf) -> (proplists:get_value(id2ftp_result,Conf))(ids(Id)).

ids([[_|_]|_]=Ids) -> filename:join(Ids);
ids(Id) -> Id.


is_expected_absName(Id, File, Conf) -> File = (proplists:get_value(id2abs,Conf))(Id).
is_expected_ftpInName(Id, File, Conf) -> File = (proplists:get_value(id2ftp,Conf))(Id).
is_expected_ftpOutName(Id, File, Conf) -> File = (proplists:get_value(id2ftp_result,Conf))(Id).


progress(#progress{} = Progress , _File, {file_size, Total}) ->
    progress_report_receiver ! start,
    Progress#progress{total = Total};

progress(#progress{total = Total, current = Current} 
	 = Progress, _File, {transfer_size, 0}) ->
    progress_report_receiver ! finish,
    case Total of
	unknown ->
	    ok;
	Current ->
	    ok;
	_  ->
	    ct:fail({error, {progress, {total, Total},
				      {current, Current}}})
    end,
    Progress;
progress(#progress{current = Current} = Progress, _File, 
	 {transfer_size, Size}) ->
    progress_report_receiver ! update,
    Progress#progress{current = Current + Size}.

progress_report_receiver_init(Pid, N) ->
    register(progress_report_receiver, self()),
    receive
	start ->
	    ok
    end,
    progress_report_receiver_loop(Pid, N-1).

progress_report_receiver_loop(Pid, N) ->
      receive
	  update ->
	    progress_report_receiver_loop(Pid, N);
	  finish when N =:= 0 ->
	      Pid ! {self(), ok};
	  finish  ->
	      Pid ! {self(), ok},
	      receive
		  start ->
		      ok
	      end,
	      progress_report_receiver_loop(Pid, N-1)
      end.

kill_ftp_proc_6035(Pid, LogFile) ->
    receive
	open ->
	    exit(Pid, shutdown),
	    kill_ftp_proc_6035(Pid, LogFile);
	{open_failed, Reason} ->
	    exit({skip, {failed_openening_server_connection, Reason}})
    after
	5000 ->
	    is_error_report_6035(LogFile)
    end.

open_wait_6035({_Tag, FtpServer}, From) ->
    case ftp:open(FtpServer, [{timeout, timer:seconds(15)}]) of
	{ok, Pid} ->
	    _LoginResult = ftp:user(Pid,"anonymous","kldjf"),
	    From ! open,
	    receive
		dummy -> 
		    ok
	    after
		10000 ->
		    ok
	    end,
	    ok;
	{error, Reason} ->
	    From ! {open_failed, {Reason, FtpServer}},
	    ok
    end.

is_error_report_6035(LogFile) ->
    Res =
	case file:read_file(LogFile) of
	    {ok, Bin} ->
		Txt = binary_to_list(Bin), 
		read_log_6035(Txt);
	    _ ->
		false
	end,
    %% file:delete(LogFile),
    Res.

read_log_6035("=ERROR REPORT===="++_Rest) ->
    true;
read_log_6035([_|T]) ->
    read_log_6035(T);
read_log_6035([]) ->
    false.
