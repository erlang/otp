%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(erl_prim_loader_SUITE).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_group/2,end_per_group/2]).

-export([get_path/1, set_path/1, get_file/1, normalize_and_backslash/1,
	 inet_existing/1, inet_coming_up/1, inet_disconnects/1,
	 multiple_slaves/1, file_requests/1,
	 local_archive/1, remote_archive/1,
	 primary_archive/1, virtual_dir_in_archive/1,
	 get_modules/1]).

-define(PRIM_FILE, prim_file).

%%-----------------------------------------------------------------
%% Test suite for erl_prim_loader. (Most code is run during system start/stop.)
%%-----------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,3}}].

all() -> 
    [get_path, set_path, get_file,
     normalize_and_backslash, inet_existing,
     inet_coming_up, inet_disconnects, multiple_slaves,
     file_requests, local_archive, remote_archive,
     primary_archive, virtual_dir_in_archive,
     get_modules].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

get_path(Config) when is_list(Config) ->
    case erl_prim_loader:get_path() of
	{ok, Path} when is_list(Path) ->
	    ok;
	_ ->
	    ct:fail(get_path)
    end,
    ok.

set_path(Config) when is_list(Config) ->
    {ok, Path} = erl_prim_loader:get_path(),
    ok = erl_prim_loader:set_path(Path),
    {ok, Path} = erl_prim_loader:get_path(),
    NewPath = Path ++ ["dummy_dir","/dummy_dir/dummy_dir"],
    ok = erl_prim_loader:set_path(NewPath),
    {ok, NewPath} = erl_prim_loader:get_path(),

    ok = erl_prim_loader:set_path(Path), % Reset path.
    {ok, Path} = erl_prim_loader:get_path(),

    {'EXIT',_} = (catch erl_prim_loader:set_path(not_a_list)),
    {ok, Path} = erl_prim_loader:get_path(),
    ok.

get_file(Config) when is_list(Config) ->
    case erl_prim_loader:get_file("lists" ++ code:objfile_extension()) of
	{ok,Bin,File} when is_binary(Bin), is_list(File) ->
	    ok;
	_ ->
	    ct:fail(get_valid_file)
    end,
    error = erl_prim_loader:get_file("duuuuuuummmy_file"),
    error = erl_prim_loader:get_file(duuuuuuummmy_file),
    error = erl_prim_loader:get_file({dummy}),
    ok.

get_modules(Config) ->
    case test_server:is_cover() of
	false -> do_get_modules(Config);
	true -> {skip,"Cover"}
    end.

do_get_modules(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    NotADir = atom_to_list(?FUNCTION_NAME) ++ "_not_a_dir",
    ok = file:write_file(filename:join(PrivDir, NotADir), <<>>),
    ok = file:set_cwd(PrivDir),

    MsGood = lists:sort([lists,gen_server,gb_trees,code_server]),
    Ms = [certainly_not_existing|MsGood],
    SuccExp = [begin
		   F = code:which(M),
		   {ok,Code} = file:read_file(F),
		   {M,{F,erlang:md5(Code)}}
	       end || M <- MsGood],
    FailExp = [{certainly_not_existing,enoent}],

    io:format("SuccExp = ~p\n", [SuccExp]),
    io:format("FailExp = ~p\n", [FailExp]),

    Path = code:get_path(),
    Process = fun(_, F, Code) -> {ok,{F,erlang:md5(Code)}} end,
    {ok,{SuccExp,FailExp}} = get_modules_sorted(Ms, Process, Path),

    %% Test that an 'enotdir' error can be handled.
    {ok,{SuccExp,FailExp}} = get_modules_sorted(Ms, Process, [NotADir|Path]),

    Name = inet_get_modules,
    {ok, Node, BootPid} = complete_start_node(Name),
    ThisDir = filename:dirname(code:which(?MODULE)),
    true = rpc:call(Node, code, add_patha, [ThisDir]),
    _ = rpc:call(Node, code, ensure_loaded, [?MODULE]),
    {ok,{InetSucc,FailExp}} = rpc:call(Node, erl_prim_loader,
				       get_modules, [Ms,Process,Path]),
    SuccExp = lists:sort(InetSucc),

    stop_node(Node),
    unlink(BootPid),
    exit(BootPid, kill),

    ok.

get_modules_sorted(Ms, Process, Path) ->
    case erl_prim_loader:get_modules(Ms, Process, Path) of
	{ok,{Succ,FailExp}} ->
	    {ok,{lists:sort(Succ),lists:sort(FailExp)}};
	Other ->
	    Other
    end.

normalize_and_backslash(Config) ->
    %% Test OTP-11170
    case os:type() of
	{win32,_} ->
	    {skip, "not on windows"};
	_ ->
	    test_normalize_and_backslash(Config)
    end.
test_normalize_and_backslash(Config) ->
    PrivDir = proplists:get_value(priv_dir,Config),
    Dir = filename:join(PrivDir,"\\"),
    File = filename:join(Dir,"file-OTP-11170"),
    ok = file:make_dir(Dir),
    ok = file:write_file(File,"a file to test OTP-11170"),
    {ok,["file-OTP-11170"]} = file:list_dir(Dir),
    {ok,["file-OTP-11170"]} = erl_prim_loader:list_dir(Dir),
    ok = file:delete(File),
    ok = file:del_dir(Dir),
    ok.

%% Start a node using the 'inet' loading method,
%% from an already started boot server.
inet_existing(Config) when is_list(Config) ->
    Name = erl_prim_test_inet_existing,
    BootPid = start_boot_server(),
    Node = start_node_using_inet(Name),
    {ok,[["inet"]]} = rpc:call(Node, init, get_argument, [loader]),
    stop_node(Node),
    unlink(BootPid),
    exit(BootPid, kill),
    ok.

%% Start a node using the 'inet' loading method,
%% but start the boot server afterwards.
inet_coming_up(Config) when is_list(Config) ->
    Name = erl_prim_test_inet_coming_up,
    Node = start_node_using_inet(Name, [{wait,false}]),

    %% Wait a while, then start boot server, and wait for node to start.
    ct:sleep({seconds,6}),
    BootPid = start_boot_server(),
    wait_really_started(Node, 25),

    %% Check loader argument, then cleanup.
    {ok,[["inet"]]} = rpc:call(Node, init, get_argument, [loader]),
    stop_node(Node),
    unlink(BootPid),
    exit(BootPid, kill),
    ok.

wait_really_started(Node, 0) ->
    ct:fail({not_booted,Node});
wait_really_started(Node, N) ->
    case rpc:call(Node, init, get_status, []) of
 	{started, _} ->
 	    ok;
	_ ->
	    ct:sleep(1000),
 	    wait_really_started(Node, N - 1)
    end.

%% Start a node using the 'inet' loading method,
%% then lose the connection.
inet_disconnects(Config) when is_list(Config) ->
    case test_server:is_native(erl_boot_server) of
	true ->
	    {skip,"erl_boot_server is native"};
	false ->
	    Name = erl_prim_test_inet_disconnects,

	    BootPid = start_boot_server(),
	    unlink(BootPid),
	    Self = self(),
	    %% This process shuts down the boot server during loading.
	    Stopper = spawn_link(fun() -> stop_boot(BootPid, Self) end),
	    receive
		{Stopper,ready} -> ok
	    end,

	    %% Let the loading begin...
	    Node = start_node_using_inet(Name, [{wait,false}]),

	    %% When the stopper is ready, the slave node should be
	    %% looking for a boot server again.
	    receive 
		{Stopper,ok} -> 
		    ok;
		{Stopper,{error,Reason}} ->
		    ct:fail(Reason)
	    after 60000 -> 
		    ct:fail(stopper_died)
	    end,

	    %% Start new boot server to see that loading is continued.
	    BootPid2 = start_boot_server(),
	    wait_really_started(Node, 25),
	    {ok,[["inet"]]} = rpc:call(Node, init, get_argument, [loader]),
	    stop_node(Node),
	    unlink(BootPid2),
	    exit(BootPid2, kill),
	    ok
    end.

%% Trace boot server calls and stop the server before loading is finished.
stop_boot(BootPid, Super) ->
    erlang:trace(all, true, [call]),
    1 = erlang:trace_pattern({erl_boot_server,send_file_result,3}, true, [local]),
    BootRef = erlang:monitor(process, BootPid),
    Super ! {self(),ready},
    Result = get_calls(100, BootPid),
    exit(BootPid, kill),
    erlang:trace_pattern({erl_boot_server,send_file_result,3}, false, [local]),
    erlang:trace(all, false, [call]),
    receive
	{'DOWN',BootRef,_,_, killed} -> ok
    end,
    Super ! {self(),Result}.    

get_calls(0, _) ->
    ok;
get_calls(Count, Pid) ->
    receive
	{trace,_,call,_MFA} ->
	    get_calls(Count-1, Pid)
    after 10000 ->
	    {error,{trace_msg_timeout,Count}}
    end.	

%% Start nodes in parallel, all using the 'inet' loading method;
%% verify that the boot server manages.
multiple_slaves(Config) when is_list(Config) ->
    Name = erl_prim_test_multiple_slaves,
    Host = host(),
    IpStr = ip_str(Host),
    Args = " -loader inet -hosts " ++ IpStr,

    NoOfNodes = 10,			% no of slave nodes to be started

    NamesAndNodes = 
        lists:map(fun(N) ->
                          NameN = atom_to_list(Name) ++ 
                              integer_to_list(N),
                          NodeN = NameN ++ "@" ++ Host,
                          {list_to_atom(NameN),list_to_atom(NodeN)}
                  end, lists:seq(1, NoOfNodes)),

    Nodes = start_multiple_nodes(NamesAndNodes, Args, []),

    %% "queue up" the nodes to wait for the boot server to respond
    %% (note: test_server supervises each node start by accept()
    %% on a socket, the timeout value for the accept has to be quite 
    %% long for this test to work).
    ct:sleep({seconds,5}),
    %% start the code loading circus!
    BootPid = start_boot_server(),
    %% give the nodes a chance to boot up before attempting to stop them
    ct:sleep({seconds,10}),

    wait_and_shutdown(lists:reverse(Nodes), 30),

    unlink(BootPid),
    exit(BootPid, kill),
    ok.

start_multiple_nodes([{Name,Node} | NNs], Args, Started) ->
    {ok,Node} = start_node(Name, Args, [{wait, false}]),
    start_multiple_nodes(NNs, Args, [Node | Started]);
start_multiple_nodes([], _, Nodes) ->
    Nodes.

wait_and_shutdown([Node | Nodes], Tries) ->
    wait_really_started(Node, Tries),
    {ok,[["inet"]]} = rpc:call(Node, init, get_argument, [loader]),
    stop_node(Node),
    wait_and_shutdown(Nodes, Tries);
wait_and_shutdown([], _) ->
    ok.


%% Start a node using the 'inet' loading method,
%% verify that the boot server responds to file requests.
file_requests(Config) when is_list(Config) ->
    {ok, Node, BootPid} = complete_start_node(erl_prim_test_file_req),

    %% compare with results from file server calls (the
    %% boot server uses the same file sys and cwd)
    {ok,Files} = file:list_dir("."),
    io:format("Files: ~p~n",[Files]),
    {ok,Files} = rpc:call(Node, erl_prim_loader, list_dir, ["."]),
    {ok,Info} = file:read_file_info(code:which(test_server)),
    {ok,Info} = rpc:call(Node, erl_prim_loader, read_file_info,
			 [code:which(test_server)]),

    PrivDir = proplists:get_value(priv_dir,Config),
    Dir = filename:join(PrivDir,?MODULE_STRING++"_file_requests"),
    ok = file:make_dir(Dir),
    Alias = filename:join(Dir,"symlink"),
    case file:make_symlink(code:which(test_server), Alias) of
	{error, enotsup} ->
	    %% Links not supported on this platform
	    ok;
	{error, eperm} ->
	    {win32,_} = os:type(),
	    %% Windows user not privileged to create symlinks"
	    ok;
	ok ->
	    %% Reading file info for link should return file info for
	    %% link target
	    {ok,Info} = rpc:call(Node, erl_prim_loader, read_file_info,
				 [Alias]),
	    #file_info{type=regular} = Info,
	    {ok,#file_info{type=symlink}} =
		rpc:call(Node, erl_prim_loader, read_link_info,
			 [Alias])
    end,

    {ok,Cwd} = file:get_cwd(),
    {ok,Cwd} = rpc:call(Node, erl_prim_loader, get_cwd, []),
    case file:get_cwd("C:") of
	{error,enotsup} ->
	    ok;
	{ok,DCwd} ->
	    {ok,DCwd} = rpc:call(Node, erl_prim_loader, get_cwd, ["C:"])
    end,

    stop_node(Node),
    unlink(BootPid),
    exit(BootPid, kill),
    ok.

%% Read files from local archive.
local_archive(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    KernelDir = filename:basename(code:lib_dir(kernel)),
    Archive = filename:join([PrivDir, KernelDir ++ init:archive_extension()]),
    file:delete(Archive),
    {ok, Archive} = create_archive(Archive, [KernelDir]),

    Node = node(),
    BeamName = "inet.beam",
    ok = test_archive(Node, Archive, KernelDir, BeamName),

    %% Cleanup
    ok = rpc:call(Node, erl_prim_loader, purge_archive_cache, []),
    ok = file:delete(Archive),
    ok.

%% Read files from remote archive.
remote_archive(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    KernelDir = filename:basename(code:lib_dir(kernel)),
    Archive = filename:join([PrivDir, KernelDir ++ init:archive_extension()]),
    file:delete(Archive),
    {ok, Archive} = create_archive(Archive, [KernelDir]),

    {ok, Node, BootPid} = complete_start_node(remote_archive),

    BeamName = "inet.beam",
    ok = test_archive(Node, Archive, KernelDir, BeamName),

    %% Cleanup
    stop_node(Node),
    unlink(BootPid),
    exit(BootPid, kill),
    ok.

%% Read files from primary archive.
primary_archive(Config) when is_list(Config) ->
    %% Copy the orig files to priv_dir
    PrivDir = proplists:get_value(priv_dir, Config),
    Archive = filename:join([PrivDir, "primary_archive.zip"]),
    file:delete(Archive),
    DataDir = proplists:get_value(data_dir, Config),
    {ok, _} = zip:create(Archive, ["primary_archive"],
			 [{compress, []}, {cwd, DataDir}]),
    {ok, _} = zip:extract(Archive, [{cwd, PrivDir}]),
    TopDir = filename:join([PrivDir, "primary_archive"]),

    %% Compile the code
    DictDir = "primary_archive_dict-1.0",
    DummyDir = "primary_archive_dummy",
    ok = compile_app(TopDir, DictDir),
    ok = compile_app(TopDir, DummyDir),

    %% Create the archive
    {ok, TopFiles} = file:list_dir(TopDir),
    {ok, {_, ArchiveBin}} = zip:create(Archive, TopFiles,
				       [memory, {compress, []}, {cwd, TopDir}]),

    %% Use temporary node to simplify cleanup
    Cookie = atom_to_list(erlang:get_cookie()),
    Args = " -setcookie " ++ Cookie,
    {ok,Node} = start_node(primary_archive, Args),
    wait_really_started(Node, 25),
    {_,_,_} = rpc:call(Node, erlang, date, []),

    %% Set primary archive 
    ExpectedEbins = [Archive, DictDir ++ "/ebin", DummyDir ++ "/ebin"],
    io:format("ExpectedEbins: ~p\n", [ExpectedEbins]),
    {ok, FileInfo} = ?PRIM_FILE:read_file_info(Archive),
    {ok, Ebins} = rpc:call(Node, erl_prim_loader, set_primary_archive,
			   [Archive, ArchiveBin, FileInfo,
			    fun escript:parse_file/1]),
    ExpectedEbins = lists:sort(Ebins), % assert

    {ok, TopFiles2} = rpc:call(Node, erl_prim_loader, list_dir, [Archive]),
    [DictDir, DummyDir] = lists:sort(TopFiles2),
    BeamName = "primary_archive_dict_app.beam",
    ok = test_archive(Node, Archive, DictDir, BeamName),

    %% Cleanup
    {ok, []} = rpc:call(Node, erl_prim_loader, set_primary_archive,
			[undefined, undefined, undefined,
			 fun escript:parse_file/1]),
    stop_node(Node),
    ok = file:delete(Archive),
    ok.

test_archive(Node, TopDir, AppDir, BeamName) ->
    %% List dir
    io:format("test_archive: ~p\n", [rpc:call(Node, erl_prim_loader, list_dir, [TopDir])]),
    {ok, TopFiles} = rpc:call(Node, erl_prim_loader, list_dir, [TopDir]),
    true = lists:member(AppDir, TopFiles),
    AbsAppDir = TopDir ++ "/" ++ AppDir,
    {ok, AppFiles} = rpc:call(Node, erl_prim_loader, list_dir, [AbsAppDir]),
    true = lists:member("ebin", AppFiles),
    Ebin = AbsAppDir ++ "/ebin",
    {ok, EbinFiles} = rpc:call(Node, erl_prim_loader, list_dir, [Ebin]),
    Beam = Ebin ++ "/" ++ BeamName,
    true = lists:member(BeamName, EbinFiles),
    error = rpc:call(Node, erl_prim_loader, list_dir, [TopDir ++ "/no_such_file"]),
    error = rpc:call(Node, erl_prim_loader, list_dir, [TopDir ++ "/ebin/no_such_file"]),

    %% File info
    {ok, #file_info{type = directory}} =
	rpc:call(Node, erl_prim_loader, read_file_info, [TopDir]),
    {ok, #file_info{type = directory}} =
	rpc:call(Node, erl_prim_loader, read_file_info, [Ebin]),
    {ok, #file_info{type = regular} = FI}  =
	rpc:call(Node, erl_prim_loader, read_file_info, [Beam]),
    error = rpc:call(Node, erl_prim_loader, read_file_info, [TopDir ++ "/no_such_file"]),
    error = rpc:call(Node, erl_prim_loader, read_file_info, [TopDir ++ "/ebin/no_such_file"]),

    %% Get file
    {ok, Bin, Beam} = rpc:call(Node, erl_prim_loader, get_file, [Beam]),
    if
	FI#file_info.size =:= byte_size(Bin) -> ok;
	true -> exit({FI#file_info.size, byte_size(Bin)})
    end,
    error = rpc:call(Node, erl_prim_loader, get_file, ["/no_such_file"]),
    error = rpc:call(Node, erl_prim_loader, get_file, ["/ebin/no_such_file"]),
    ok.

create_archive(Archive, AppDirs) ->
    LibDir = code:lib_dir(),
    Opts = [{compress, []}, {cwd, LibDir}],
    io:format("zip:create(~p,\n\t~p,\n\t~p).\n", [Archive, AppDirs, Opts]),
    zip:create(Archive, AppDirs, Opts).


%% Read virtual directories from archive.
virtual_dir_in_archive(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Data = <<"A little piece of data.">>,
    ArchiveBase = "archive_with_virtual_dirs",
    Archive = filename:join([PrivDir, ArchiveBase ++ init:archive_extension()]),
    FileBase = "a_data_file.beam",
    EbinBase = "ebin",
    FileInArchive = filename:join([ArchiveBase, EbinBase, FileBase]),
    BinFiles = [{FileInArchive, Data}],
    Opts = [{compress, []}],
    file:delete(Archive),
    io:format("zip:create(~p,\n\t~p,\n\t~p).\n", [Archive, BinFiles, Opts]),
    {ok, Archive} = zip:create(Archive, BinFiles, Opts),

    %% Verify that there is no directories
    {ok, BinFiles} = zip:unzip(Archive, [memory]),

    FullPath = filename:join([Archive, FileInArchive]),
    {ok, _} = erl_prim_loader:read_file_info(FullPath),

    %% Read one virtual dir
    EbinDir = filename:dirname(FullPath),
    {ok, _} = erl_prim_loader:read_file_info(EbinDir),
    {ok, [FileBase]} = erl_prim_loader:list_dir(EbinDir),

    %% Read another virtual dir
    AppDir = filename:dirname(EbinDir),
    {ok, _} = erl_prim_loader:read_file_info(AppDir),
    {ok, [EbinBase]} = erl_prim_loader:list_dir(AppDir),

    %% Cleanup
    ok = erl_prim_loader:purge_archive_cache(),
    ok = file:delete(Archive),
    ok.

%%%
%%% Helper functions.
%%%

complete_start_node(Name) ->
    BootPid = start_boot_server(),
    Node = start_node_using_inet(Name),
    wait_really_started(Node, 25),
    {ok, Node, BootPid}.

start_boot_server() ->
    %% Many linux systems define:
    %%   127.0.0.1 localhost
    %%   127.0.1.1 somehostname
    %% Therefore, to allow the tests to work on those kind of systems,
    %% also include "localhost" in the list of allowed hosts.

    Hosts = [host(),ip_str("localhost")],
    {ok,BootPid} = erl_boot_server:start_link(Hosts),
    BootPid.

start_node_using_inet(Name) ->
    start_node_using_inet(Name, []).

start_node_using_inet(Name, Opts) ->
    Host = host(),
    IpStr = ip_str(Host),
    Args = " -loader inet -hosts " ++ IpStr,
    {ok,Node} = start_node(Name, Args, Opts),
    Node.


ip_str({A, B, C, D}) ->
    lists:concat([A, ".", B, ".", C, ".", D]);
ip_str(Host) ->
    {ok,Ip} = inet:getaddr(Host, inet),
    ip_str(Ip).

start_node(Name, Args) ->
    start_node(Name, Args, []).

start_node(Name, Args, Opts) ->
    Opts2 = [{args, Args}|Opts],
    io:format("test_server:start_node(~p, peer, ~p).\n",
	      [Name, Opts2]),
    Res = test_server:start_node(Name, peer, Opts2),
    io:format("start_node -> ~p\n", [Res]),
    Res.

host() ->
    {ok,Host} = inet:gethostname(),
    Host.

stop_node(Node) ->
    test_server:stop_node(Node).

compile_app(TopDir, AppName) ->
    AppDir = filename:join([TopDir, AppName]),
    SrcDir = filename:join([AppDir, "src"]),
    OutDir = filename:join([AppDir, "ebin"]),
    {ok, Files} = file:list_dir(SrcDir),
    compile_files(Files, SrcDir, OutDir).

compile_files([File | Files], SrcDir, OutDir) ->
    case filename:extension(File) of
	".erl" ->
	    AbsFile = filename:join([SrcDir, File]),
	    case compile:file(AbsFile, [{outdir, OutDir}]) of
		{ok, _Mod} ->
		    compile_files(Files, SrcDir, OutDir);
		Error ->
		    {compilation_error, AbsFile, OutDir, Error}
	    end;
	_ ->
	    compile_files(Files, SrcDir, OutDir)
    end;
compile_files([], _, _) ->
    ok.

