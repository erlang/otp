%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2017. All Rights Reserved.
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
-module(upgrade_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(upgr_sname,otp_upgrade).

%% Applications that are excluded from this test because they can not
%% just be started in a new node with out specific configuration.
-define(start_exclude,
	[cosEvent,cosEventDomain,cosFileTransfer,cosNotification,
	 cosProperty,cosTime,cosTransactions,erts,ic,netconf,orber,
	 safe]).

%% Applications that are excluded from this test because their appup
%% file don't support the upgrade.
%% In specific:
%% - hipe does not support any upgrade at all
%% - dialyzer requires hipe (in the .app file)
%% - erl_interface, jinterface support no upgrade
-define(appup_exclude, 
	[dialyzer,hipe,typer,erl_interface,jinterface,ose]).

init_per_suite(Config) ->
    %% Check that a real release is running, not e.g. cerl
    ok = application:ensure_started(sasl),
    case release_handler:which_releases() of
	[{_,_,[],_}] ->
	    %% Fake release, no applications
	    {skip, "Need a real release running to create other releases"};
	_ ->
	    rm_rf(filename:join([proplists:get_value(data_dir,Config),priv_dir])),
	    Config
    end.

end_per_suite(_Config) ->
    %% This function is required since init_per_suite/1 exists.
    ok.

init_per_testcase(Case,Config) ->
    PrivDir = filename:join([proplists:get_value(data_dir,Config),priv_dir,Case]),
    CreateDir = filename:join([PrivDir,create]),
    InstallDir = filename:join([PrivDir,install]),
    ok = filelib:ensure_dir(filename:join(CreateDir,"*")),
    ok = filelib:ensure_dir(filename:join(InstallDir,"*")),
    Config1 = lists:keyreplace(priv_dir,1,Config,{priv_dir,PrivDir}),
    [{create_dir,CreateDir},{install_dir,InstallDir}|Config1].

end_per_testcase(_Case,Config) ->
    Nodes = nodes(),
    [test_server:stop_node(Node) || Node <- Nodes],
    case proplists:get_value(tc_status,Config) of
	ok ->
	    %% Note that priv_dir here is per test case!
	    rm_rf(proplists:get_value(priv_dir,Config));
	_fail ->
	    %% Test case data can be found under DataDir/priv_dir/Case
	    ok
    end,
    ok.

all() ->
    [minor,major].

%% If this is major release X, then this test performs an upgrade from
%% major release X-1 to the current release.
major(Config) ->
    Current = erlang:system_info(otp_release),
    PreviousMajor = previous_major(Current),
    upgrade_test(PreviousMajor,Current,Config).

%% If this is a patched version of major release X, then this test
%% performs an upgrade from major release X to the current release.
minor(Config) ->
    CurrentMajor = erlang:system_info(otp_release),
    Current = CurrentMajor++"_patched",
    upgrade_test(CurrentMajor,Current,Config).

%%%-----------------------------------------------------------------
upgrade_test(FromVsn,ToVsn,Config) ->
    OldRel =
	case test_server:is_release_available(FromVsn) of
	    true ->
		{release,FromVsn};
	    false ->
		case ct:get_config({otp_releases,list_to_atom(FromVsn)}) of
		    undefined ->
			false;
		    Prog0 ->
			case os:find_executable(Prog0) of
			    false ->
				false;
			    Prog ->
				{prog,Prog}
			end
		end
	end,
    case OldRel of
	false ->
	    %% Note that priv_dir here is per test case!
	    rm_rf(proplists:get_value(priv_dir,Config)),
	    {skip, "no previous release available"};
	_ ->
	    upgrade_test1(FromVsn,ToVsn,[{old_rel,OldRel}|Config])
    end.

upgrade_test1(FromVsn,ToVsn,Config) ->
    CreateDir = proplists:get_value(create_dir,Config),
    InstallDir = proplists:get_value(install_dir,Config),
    FromRelName = "otp-"++FromVsn,
    ToRelName = "otp-"++ToVsn,

    {FromRel,FromApps} = target_system(FromRelName, FromVsn,
				       CreateDir, InstallDir,Config),
    {ToRel,ToApps} = upgrade_system(FromRel, ToRelName, ToVsn,
				    CreateDir, InstallDir),
    do_upgrade(FromVsn, FromApps, ToRel, ToApps, InstallDir).

%%%-----------------------------------------------------------------
%%% This is similar to sasl/examples/src/target_system.erl, but with
%%% the following adjustments:
%%% - add a log directory
%%% - use an own 'start' script
%%% - chmod 'start' and 'start_erl'
target_system(RelName0,RelVsn,CreateDir,InstallDir,Config) ->
    {ok,Node} = test_server:start_node(list_to_atom(RelName0),peer,
				       [{erl,[proplists:get_value(old_rel,Config)]}]),

    {RelName,Apps,ErtsVsn} = create_relfile(Node,CreateDir,RelName0,RelVsn),
 
    %% Create .script and .boot
    ok = rpc:call(Node,systools,make_script,[RelName]),
    
    %% Create base tar file - i.e. erts and all apps
    ok = rpc:call(Node,systools,make_tar,
		  [RelName,[{erts,rpc:call(Node,code,root_dir,[])}]]),
    
    %% Unpack the tar to complete the installation
    erl_tar:extract(RelName ++ ".tar.gz", [{cwd, InstallDir}, compressed]),
    
    %% Add bin and log dirs
    BinDir = filename:join([InstallDir, "bin"]),
    file:make_dir(BinDir),
    file:make_dir(filename:join(InstallDir,"log")),

    %% Delete start scripts - they will be added later
    ErtsBinDir = filename:join([InstallDir, "erts-" ++ ErtsVsn, "bin"]),
    file:delete(filename:join([ErtsBinDir, "erl"])),
    file:delete(filename:join([ErtsBinDir, "start"])),
    file:delete(filename:join([ErtsBinDir, "start_erl"])),
    
    %% Copy .boot to bin/start.boot
    copy_file(RelName++".boot",filename:join([BinDir, "start.boot"])),
    
    %% Copy scripts from erts-xxx/bin to bin
    copy_file(filename:join([ErtsBinDir, "epmd"]), 
              filename:join([BinDir, "epmd"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "run_erl"]), 
              filename:join([BinDir, "run_erl"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "to_erl"]), 
              filename:join([BinDir, "to_erl"]), [preserve]),
    
    %% create start_erl.data and sys.config
    StartErlData = filename:join([InstallDir, "releases", "start_erl.data"]),
    write_file(StartErlData, io_lib:fwrite("~s ~s~n", [ErtsVsn, RelVsn])),
    SysConfig = filename:join([InstallDir, "releases", RelVsn, "sys.config"]),
    write_file(SysConfig, "[]."),
    
    %% Insert 'start' script from data_dir - modified to add sname and heart
    copy_file(filename:join(proplists:get_value(data_dir,Config),"start.src"),
	      filename:join(ErtsBinDir,"start.src")),
    ok = file:change_mode(filename:join(ErtsBinDir,"start.src"),8#0755),

    %% Make start_erl executable
    %% (this has been fixed in OTP 17 - is is now installed with
    %% $INSTALL_SCRIPT instead of $INSTALL_DATA and should therefore
    %% be executable from the start)
    ok = file:change_mode(filename:join(ErtsBinDir,"start_erl.src"),8#0755),

    %% Substitute variables in erl.src, start.src and start_erl.src
    %% (.src found in erts-xxx/bin - result stored in bin)
    subst_src_scripts(["erl", "start", "start_erl"], ErtsBinDir, BinDir, 
                      [{"FINAL_ROOTDIR", InstallDir}, {"EMU", "beam"}],
                      [preserve]),

    %% Create RELEASES
    RelFile = filename:join([InstallDir, "releases", 
			     filename:basename(RelName) ++ ".rel"]),
    release_handler:create_RELEASES(InstallDir, RelFile),

    true = test_server:stop_node(Node),

    {RelName,Apps}.

%%%-----------------------------------------------------------------
%%% Create a release containing the current (the test node) OTP
%%% release, including relup to allow upgrade from an earlier OTP
%%% release.
upgrade_system(FromRel, ToRelName0, ToVsn,
	       CreateDir, InstallDir) ->

    {RelName,Apps,_} = create_relfile(node(),CreateDir,ToRelName0,ToVsn),
    FromPath = filename:join([InstallDir,lib,"*",ebin]),

    ok = systools:make_script(RelName),
    ok = systools:make_relup(RelName,[FromRel],[FromRel],
			     [{path,[FromPath]},
			      {outdir,CreateDir}]),
    SysConfig = filename:join([CreateDir, "sys.config"]),
    write_file(SysConfig, "[]."),

    ok = systools:make_tar(RelName,[{erts,code:root_dir()}]),

    {RelName,Apps}.

%%%-----------------------------------------------------------------
%%% Start a new node running the release from target_system/5
%%% above. Then upgrade to the system from upgrade_system/5.
do_upgrade(FromVsn,FromApps,ToRel,ToApps,InstallDir) ->
    Start = filename:join([InstallDir,bin,start]),
    {ok,Node} = start_node(Start,permanent,FromVsn,FromApps),

    [{"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    ToRelName = filename:basename(ToRel),
    copy_file(ToRel++".tar.gz",
	      filename:join([InstallDir,releases,ToRelName++".tar.gz"])),
    {ok,ToVsn} = rpc:call(Node,release_handler,unpack_release,[ToRelName]),
    [{"OTP upgrade test",ToVsn,_,unpacked},
     {"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    case rpc:call(Node,release_handler,install_release,[ToVsn]) of
	{ok,FromVsn,_} ->
	    ok;
	{continue_after_restart,FromVsn,_} ->
	    wait_node_up(current,ToVsn,ToApps)
    end,
    [{"OTP upgrade test",ToVsn,_,current},
     {"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    ok = rpc:call(Node,release_handler,make_permanent,[ToVsn]),
    [{"OTP upgrade test",ToVsn,_,permanent},
     {"OTP upgrade test",FromVsn,_,old}] =
	rpc:call(Node,release_handler,which_releases,[]),
    
    erlang:monitor_node(Node,true),
    _ = rpc:call(Node,init,stop,[]),
    receive {nodedown,Node} -> ok end,

    ok.

%%%-----------------------------------------------------------------
%%% Library functions
previous_major("17") ->
    "r16b";
previous_major(Rel) ->
    integer_to_list(list_to_integer(Rel)-1).

create_relfile(Node,CreateDir,RelName0,RelVsn) ->
    LibDir = rpc:call(Node,code,lib_dir,[]),
    SplitLibDir = filename:split(LibDir),
    Paths = rpc:call(Node,code,get_path,[]),
    Exclude = ?start_exclude ++ ?appup_exclude,
    Apps = lists:flatmap(
	     fun(Path) ->
		     case lists:prefix(LibDir,Path) of
			 true ->
			     case filename:split(Path) -- SplitLibDir of
				 [AppVsn,"ebin"] ->
				     case string:lexemes(AppVsn,"-") of
					 [AppStr,Vsn] ->
					     App = list_to_atom(AppStr),
					     case lists:member(App,Exclude) of
						 true ->
						     [];
						 false ->
						     [{App,Vsn,restart_type(App)}]
					     end;
					 _ ->
					     []
				     end;
				 _ ->
				     []
			     end;
			 false ->
			     []
		     end
	     end,
	     Paths),

    ErtsVsn = rpc:call(Node, erlang, system_info, [version]),
    
    %% Create the .rel file
    RelContent = {release, {"OTP upgrade test", RelVsn}, {erts, ErtsVsn}, Apps},
    RelName = filename:join(CreateDir,RelName0),
    RelFile = RelName++".rel",
    {ok,Fd} = file:open(RelFile,[write,{encoding,utf8}]),
    io:format(Fd,"~tp.~n",[RelContent]),
    ok = file:close(Fd),
    {RelName,Apps,ErtsVsn}.

restart_type(App) when App==kernel; App==stdlib; App==sasl ->
    permanent;
restart_type(_) ->
    temporary.

copy_file(Src, Dest) ->
    copy_file(Src, Dest, []).

copy_file(Src, Dest, Opts) ->
    {ok,_} = file:copy(Src, Dest),
    case lists:member(preserve, Opts) of
        true ->
            {ok, FileInfo} = file:read_file_info(Src),
            file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.


write_file(FName, Conts) ->
    Enc = file:native_name_encoding(),
    {ok, Fd} = file:open(FName, [write]),
    file:write(Fd, unicode:characters_to_binary(Conts,Enc,Enc)),
    file:close(Fd).


subst_src_scripts(Scripts, SrcDir, DestDir, Vars, Opts) -> 
    lists:foreach(fun(Script) ->
                          subst_src_script(Script, SrcDir, DestDir, 
                                           Vars, Opts)
                  end, Scripts).

subst_src_script(Script, SrcDir, DestDir, Vars, Opts) -> 
    subst_file(filename:join([SrcDir, Script ++ ".src"]),
               filename:join([DestDir, Script]),
               Vars, Opts).

subst_file(Src, Dest, Vars, Opts) ->
    {ok, Bin} = file:read_file(Src),
    Conts = binary_to_list(Bin),
    NConts = subst(Conts, Vars),
    write_file(Dest, NConts),
    case lists:member(preserve, Opts) of
        true ->
            {ok, FileInfo} = file:read_file_info(Src),
            file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

%% subst(Str, Vars)
%% Vars = [{Var, Val}]
%% Var = Val = string()
%% Substitute all occurrences of %Var% for Val in Str, using the list
%% of variables in Vars.
%%
subst(Str, Vars) ->
    subst(Str, Vars, []).

subst([$%, C| Rest], Vars, Result) when $A =< C, C =< $Z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when $a =< C, C =< $z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when  C == $_ ->
    subst_var([C| Rest], Vars, Result, []);
subst([C| Rest], Vars, Result) ->
    subst(Rest, Vars, [C| Result]);
subst([], _Vars, Result) ->
    lists:reverse(Result).

subst_var([$%| Rest], Vars, Result, VarAcc) ->
    Key = lists:reverse(VarAcc),
    case lists:keysearch(Key, 1, Vars) of
        {value, {Key, Value}} ->
            subst(Rest, Vars, lists:reverse(Value, Result));
        false ->
            subst(Rest, Vars, [$%| VarAcc ++ [$%| Result]])
    end;
subst_var([C| Rest], Vars, Result, VarAcc) ->
    subst_var(Rest, Vars, Result, [C| VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
    subst([], Vars, [VarAcc ++ [$%| Result]]).


%%%-----------------------------------------------------------------
%%% 
start_node(Start,ExpStatus,ExpVsn,ExpApps) ->
    case open_port({spawn_executable, Start}, []) of
        Port when is_port(Port) ->
            unlink(Port),
            erlang:port_close(Port),
	    wait_node_up(ExpStatus,ExpVsn,ExpApps);
        Error ->
            Error
    end.

wait_node_up(ExpStatus,ExpVsn,ExpApps0) ->
    ExpApps = [{A,V} || {A,V,_T} <- ExpApps0],
    Node = node_name(?upgr_sname),
    wait_node_up(Node,ExpStatus,ExpVsn,lists:keysort(1,ExpApps),60).

wait_node_up(Node,ExpStatus,ExpVsn,ExpApps,0) ->
    ct:fail({app_check_failed,ExpVsn,ExpApps,
	     rpc:call(Node,release_handler,which_releases,[ExpStatus]),
	     rpc:call(Node,application,which_applications,[])});
wait_node_up(Node,ExpStatus,ExpVsn,ExpApps,N) ->
    timer:sleep(2000),
    case {rpc:call(Node,release_handler,which_releases,[ExpStatus]),
	  rpc:call(Node, application, which_applications, [])} of
	{[{_,ExpVsn,_,_}],Apps} when is_list(Apps) ->
	    case [{A,V} || {A,_,V} <- lists:keysort(1,Apps)] of
		ExpApps -> {ok,Node};
		_ -> wait_node_up(Node,ExpStatus,ExpVsn,ExpApps,N-1)
	    end;
	_ ->
	    wait_node_up(Node,ExpStatus,ExpVsn,ExpApps,N-1)
    end.

node_name(Sname) ->
    {ok,Host} = inet:gethostname(),
    list_to_atom(atom_to_list(Sname) ++ "@" ++ Host).

rm_rf(Dir) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok, Content} = file:list_dir_all(Dir),
	    [rm_rf(filename:join(Dir,C)) || C <- Content],
	    ok=file:del_dir(Dir),
	    ok;
	{ok, #file_info{}} ->
	    ok=file:delete(Dir);
	_ ->
	    ok
    end. 
