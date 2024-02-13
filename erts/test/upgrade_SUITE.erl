%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2024. All Rights Reserved.
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

-compile(r24).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(upgr_sname,otp_upgrade).

%% Applications that are excluded from this test because they cannot
%% just be started in a new node with out specific configuration.
-define(start_exclude,
	[cosEvent,cosEventDomain,cosFileTransfer,cosNotification,
	 cosProperty,cosTime,cosTransactions,erts,ic,netconf,orber,
	 safe,wx,observer,et,debugger,dialyzer]).

%% Applications that are excluded from this test because their appup
%% file don't support the upgrade.
%% In specific:
%% - erl_interface, jinterface support no upgrade
-define(appup_exclude, [erl_interface,jinterface]).

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
    [minor,major,ancient_major].

%% If this is major release X, then this test performs an upgrade from
%% major release X-1 to the current release.
major(Config) ->
    put(verbose, true),
    p("major -> get current major release"),
    Current = erlang:system_info(otp_release),
    PreviousMajor = previous_major(Current),
    upgrade_test(PreviousMajor,Current,Config).

%% If this is major release X, then this test performs an upgrade from
%% major release X-2 to the current release.
ancient_major(Config) ->
    put(verbose, true),
    p("ancient_major -> get current major release"),
    Current = erlang:system_info(otp_release),
    PreviousPreviousMajor = previous_major(previous_major(Current)),
    upgrade_test(PreviousPreviousMajor,Current,Config).

%% If this is a patched version of major release X, then this test
%% performs an upgrade from major release X to the current release.
minor(Config) ->
    put(verbose, true),
    p("minor -> get current major release"),
    CurrentMajor = erlang:system_info(otp_release),
    Current = CurrentMajor++"_patched",
    upgrade_test(CurrentMajor,Current,Config).

%%%-----------------------------------------------------------------
upgrade_test(FromVsn,ToVsn,Config) ->
    p("upgrade_test -> entry with"
      "~n      FromVsn: ~p"
      "~n      ToVsn:   ~p"
      "~n      Config:  ~p", [FromVsn, ToVsn, Config]),
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
    p("upgrade_test -> old release: "
      "~n      ~p", [OldRel]),
    case OldRel of
	false ->
	    %% Note that priv_dir here is per test case!
	    rm_rf(proplists:get_value(priv_dir,Config)),
	    {skip, "no previous release available"};
	_ ->
	    upgrade_test1(FromVsn,ToVsn,[{old_rel,OldRel}|Config])
    end.

upgrade_test1(FromVsn,ToVsn,Config) ->
    p("upgrade_test1 -> get create-dir"),
    CreateDir = proplists:get_value(create_dir,Config),
    p("upgrade_test1 -> get install-dir"),
    InstallDir = proplists:get_value(install_dir,Config),
    FromRelName = "otp-"++FromVsn,
    ToRelName = "otp-"++ToVsn,

    {FromRel,FromApps} = target_system(FromRelName, FromVsn,
				       CreateDir, InstallDir,Config),
    p("upgrade_test1 -> target system:"
      "~n      FromRel:  ~p"
      "~n      FromApps: ~p", [FromRel, FromApps]),
    {ToRel,ToApps} = upgrade_system(FromVsn, FromRel, ToRelName, ToVsn,
				    CreateDir, InstallDir),
    p("upgrade_test1 -> upgrade system:"
      "~n      ToRel:  ~p"
      "~n      ToApps: ~p", [ToRel, ToApps]),
    do_upgrade(FromVsn, FromApps, ToRel, ToApps, InstallDir).

%%%-----------------------------------------------------------------
%%% This is similar to sasl/examples/src/target_system.erl, but with
%%% the following adjustments:
%%% - add a log directory
%%% - use an own 'start' script
%%% - chmod 'start' and 'start_erl'
target_system(RelName0,RelVsn,CreateDir,InstallDir,Config) ->
    p("target_system -> start worker node"),
    {ok,Node} = test_server:start_node(list_to_atom(RelName0),peer,
				       [{erl,[proplists:get_value(old_rel,Config)]}]),

    p("target_system -> create relfile"),
    {RelName,Apps,ErtsVsn} = create_relfile(Node,CreateDir,RelName0,RelVsn),
 
    %% Create .script and .boot
    p("target_system -> create .script and .boot"),
    ok = rpc:call(Node,systools,make_script,[RelName]),
    
    %% Create base tar file - i.e. erts and all apps
    p("target_system -> create base tar file"),
    ok = rpc:call(Node,systools,make_tar,
		  [RelName,[{erts,rpc:call(Node,code,root_dir,[])}]]),
    
    %% Unpack the tar to complete the installation
    p("target_system -> unpack to complete installation"),
    erl_tar:extract(RelName ++ ".tar.gz", [{cwd, InstallDir}, compressed]),
    
    %% Add bin and log dirs
    p("target_system -> add bin and log dirs"),
    BinDir = filename:join([InstallDir, "bin"]),
    file:make_dir(BinDir),
    file:make_dir(filename:join(InstallDir,"log")),

    %% Delete start scripts - they will be added later
    p("target_system -> delete start scripts"),
    ErtsBinDir = filename:join([InstallDir, "erts-" ++ ErtsVsn, "bin"]),
    file:delete(filename:join([ErtsBinDir, "erl"])),
    file:delete(filename:join([ErtsBinDir, "start"])),
    file:delete(filename:join([ErtsBinDir, "start_erl"])),
    
    %% Copy .boot to bin/start.boot
    p("target_system -> copy .boot and bin/start.boot"),
    copy_file(RelName++".boot",filename:join([BinDir, "start.boot"])),
    
    %% Copy scripts from erts-xxx/bin to bin
    p("target_system -> copy scripts from erts-xxx/bin to bin"),
    copy_file(filename:join([ErtsBinDir, "epmd"]), 
              filename:join([BinDir, "epmd"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "run_erl"]), 
              filename:join([BinDir, "run_erl"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "to_erl"]), 
              filename:join([BinDir, "to_erl"]), [preserve]),
    
    %% create start_erl.data and sys.config
    p("target_system -> create start_erl.data and sys.config"),
    StartErlData = filename:join([InstallDir, "releases", "start_erl.data"]),
    write_file(StartErlData, io_lib:fwrite("~s ~s~n", [ErtsVsn, RelVsn])),
    SysConfig = filename:join([InstallDir, "releases", RelVsn, "sys.config"]),
    write_file(SysConfig, "[]."),
    
    %% Insert 'start' script from data_dir - modified to add sname and heart
    p("target_system -> insert 'start' script from data-dir"),
    copy_file(filename:join(proplists:get_value(data_dir,Config),"start.src"),
	      filename:join(ErtsBinDir,"start.src")),
    ok = file:change_mode(filename:join(ErtsBinDir,"start.src"),8#0755),

    %% Make start_erl executable
    %% (this has been fixed in OTP 17 - is is now installed with
    %% $INSTALL_SCRIPT instead of $INSTALL_DATA and should therefore
    %% be executable from the start)
    p("target_system -> make start_erl executable"),
    ok = file:change_mode(filename:join(ErtsBinDir,"start_erl.src"),8#0755),

    %% Substitute variables in erl.src, start.src and start_erl.src
    %% (.src found in erts-xxx/bin - result stored in bin)
    p("target_system -> substitute variables"),
    subst_src_scripts(["erl", "start", "start_erl"], ErtsBinDir, BinDir, 
                      [{"FINAL_ROOTDIR", InstallDir}, {"EMU", "beam"}],
                      [preserve]),

    %% Create RELEASES
    p("target_system -> create releases"),
    RelFile = filename:join([InstallDir, "releases", 
			     filename:basename(RelName) ++ ".rel"]),
    release_handler:create_RELEASES(InstallDir, RelFile),

    p("target_system -> stop worker node"),
    true = test_server:stop_node(Node),

    p("target_system -> done"),
    {RelName,Apps}.


%%%-----------------------------------------------------------------
%%% Create a release containing the current (the test node) OTP
%%% release, including relup to allow upgrade from an earlier OTP
%%% release.
upgrade_system(FromVsn, FromRel, ToRelName0, ToVsn,
	       CreateDir, InstallDir) ->

    p("upgrade_system -> create relfile"),
    {RelName,Apps,_} = create_relfile(node(),CreateDir,ToRelName0,ToVsn),
    FromPath = filename:join([InstallDir,lib,"*",ebin]),

    p("upgrade_system -> make script"),
    ok = systools:make_script(RelName),
    p("upgrade_system -> make relup"),
    ok = systools:make_relup(RelName,[FromRel],[FromRel],
			     [{path,[FromPath]},
			      {outdir,CreateDir}]),

    case {FromVsn,ToVsn} of
        {"20"++_,"21"++_} ->
            p("upgrade_system -> try fix relup (with regards to inets ftp)"),
            fix_relup_inets_ftp(filename:dirname(RelName));
        _ -> ok
    end,

    p("upgrade_system -> write system conf file"),
    SysConfig = filename:join([CreateDir, "sys.config"]),
    write_file(SysConfig, "[]."),

    p("upgrade_system -> make tar"),
    ok = systools:make_tar(RelName,[{erts,code:root_dir()}]),

    p("upgrade_system -> done"),
    {RelName,Apps}.

%% In OTP-21, ftp and tftp were split out from inets and formed two
%% new separate applications. When creating the relup, systools
%% automatically adds new applications first, before upgrading
%% existing applications. Since ftp and tftp have processes with the
%% same name as in the old version of inets, the upgrade failed with
%% trying to start the new applications (already exist).
%%
%% To go around this problem, this function adds an instruction to
%% stop inets before the new applications are started. This is a very
%% specific adjustment, and it will be needed for any upgrade which
%% involves conversion from inets to ftp/tftp.
fix_relup_inets_ftp(Dir) ->
    Filename = filename:join(Dir,"relup"),
    {ok,[{ToVsn,Up,Down}]} = file:consult(Filename),
    [{FromVsn,UpDescr,UpInstr}] = Up,
    [{FromVsn,DownDescr,DownInstr}] = Down,

    Fun = fun(point_of_no_return) -> false;
             (_) -> true
          end,
    {UpBefore,[point_of_no_return|UpAfter]} = lists:splitwith(Fun,UpInstr),
    {DownBefore,[point_of_no_return|DownAfter]} = lists:splitwith(Fun,DownInstr),
    NewRelup =
        {ToVsn,
         [{FromVsn,UpDescr,UpBefore++[point_of_no_return,
                                      {apply,{application,stop,[inets]}} |
                                      UpAfter]}],
         [{FromVsn,DownDescr,DownBefore++[point_of_no_return,
                                          {apply,{application,stop,[inets]}} |
                                          DownAfter]}]},
    {ok, Fd} = file:open(Filename, [write,{encoding,utf8}]),
    io:format(Fd, "%% ~s~n~tp.~n", [epp:encoding_to_string(utf8),NewRelup]),
    ok = file:close(Fd).


%%%-----------------------------------------------------------------
%%% Start a new node running the release from target_system/5
%%% above. Then upgrade to the system from upgrade_system/5.
do_upgrade(FromVsn,FromApps,ToRel,ToApps,InstallDir) ->

    p("do_upgrade -> entry with"
      "~n      FromVsn:    ~p"
      "~n      FromApps:   ~p"
      "~n      ToRel:      ~p"
      "~n      ToApps:     ~p"
      "~n      InstallDir: ~p",
      [FromVsn, FromApps, ToRel, ToApps, InstallDir]),

    Start = filename:join([InstallDir,bin,start]),
    p("do_upgrade -> start (worker from-) node"),
    {ok,Node} = start_node(Start,permanent,FromVsn,FromApps),

    p("do_upgrade -> verify (from-) release (permanent)"),
    [{"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    ToRelName = filename:basename(ToRel),
    p("do_upgrade -> copy (to-) release"),
    copy_file(ToRel++".tar.gz",
	      filename:join([InstallDir,releases,ToRelName++".tar.gz"])),
    p("do_upgrade -> unpack (to-) release"),
    {ok,ToVsn} = rpc:call(Node,release_handler,unpack_release,[ToRelName]),
    p("do_upgrade -> verify release(s) (unpacked,permanent)"),
    [{"OTP upgrade test",ToVsn,_,unpacked},
     {"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    p("do_upgrade -> install (to-) release"),
    case rpc:call(Node,release_handler,install_release,[ToVsn]) of
	{ok,FromVsn,_} ->
	    ok;
	{continue_after_restart,FromVsn,_} ->
	    wait_node_up(current,ToVsn,ToApps)
    end,
    p("do_upgrade -> verify release(s) (current,permanent)"),
    [{"OTP upgrade test",ToVsn,_,current},
     {"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    p("do_upgrade -> make (to-) permanent"),
    ok = rpc:call(Node,release_handler,make_permanent,[ToVsn]),
    p("do_upgrade -> verify release(s) (permanent,old)"),
    [{"OTP upgrade test",ToVsn,_,permanent},
     {"OTP upgrade test",FromVsn,_,old}] =
	rpc:call(Node,release_handler,which_releases,[]),
    
    p("do_upgrade -> stop (worker) node"),
    erlang:monitor_node(Node,true),
    _ = rpc:call(Node,init,stop,[]),
    receive {nodedown,Node} -> ok end,

    p("do_upgrade -> done"),
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
    case open_port({spawn_executable, Start}, [{env,[{"ERL_AFLAGS",false}]}]) of
        Port when is_port(Port) ->
            unlink(Port),
            erlang:port_close(Port),
	    wait_node_up(ExpStatus, ExpVsn, ExpApps);
        Error ->
            Error
    end.

wait_node_up(ExpStatus, ExpVsn, ExpApps0) ->
    ExpApps = [{A,V} || {A,V,_T} <- ExpApps0],
    Node    = node_name(?upgr_sname),
    wait_node_up(Node, ExpStatus, ExpVsn, lists:keysort(1,ExpApps), 60).

wait_node_up(Node, ExpStatus, ExpVsn, ExpApps, 0) ->
    LogTxt =
        try erpc:call(Node, code, root_dir, []) of
            Root ->
                LogGlob = filename:join([Root,"log","erlang.*"]),
                {ok, Log} = case filelib:wildcard(LogGlob) of
                                [Logfile|_] ->
                                    file:read_file(Logfile);
                                [] ->
                                    {ok, "No log file found"}
                            end,
                Log
        catch C:E ->
                {"erpc:call",C,E}
        end,
    p("wait_node_up -> fail~n"
      "Logs: ~n~ts~n", [LogTxt]),
    ct:fail({app_check_failed,ExpVsn,ExpApps,
	     rpc:call(Node, release_handler, which_releases,     [ExpStatus]),
	     rpc:call(Node, application,     which_applications, [])});
wait_node_up(Node, ExpStatus, ExpVsn, ExpApps, N) ->
    timer:sleep(2000),
    p("wait_node_up -> [~w] get release vsn and apps", [N]),
    case {rpc:call(Node, release_handler, which_releases,     [ExpStatus]),
          rpc:call(Node, application,     which_applications, [])} of
        {[{_,ExpVsn,_,_}],Apps} when is_list(Apps) ->
            p("wait_node_up -> [~w] expected release vsn - check apps", [N]),
            case [{A,V} || {A,_,V} <- lists:keysort(1,Apps)] of
                ExpApps ->
                    p("wait_node_up -> [~w] expected apps", [N]),
                    {ok, Node};
                UnexpApps ->
                    p("wait_node_up -> [~w] still wrong apps:~n"
                      "Missing:~p~n"
                      "Extra:  ~p~n"
                      "All:    ~p~n"
                      , [N, ExpApps -- UnexpApps, UnexpApps -- ExpApps, UnexpApps]),
                    wait_node_up(Node, ExpStatus, ExpVsn, ExpApps, N-1)
            end;
        {[{_,Vsn,_,_}],_} ->
            p("wait_node_up -> [~w] still wrong release vsn:"
              "~n      ~p", [N, Vsn]),
            wait_node_up(Node, ExpStatus, ExpVsn, ExpApps, N-1);
        X ->
            p("wait_node_up -> [~w] unexpected results:"
              "~n      ~p", [N, X]),
            wait_node_up(Node, ExpStatus, ExpVsn, ExpApps, N-1)
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


%%%-----------------------------------------------------------------
%%% 

p(F) ->
    p(F, []).

p(F, A) ->
    p(get(verbose), F, A).

p(true, F, A) ->
    print(F, A);
p(_, _, _) ->
    ok.

print(F, A) ->
    ct:pal(F ++ "~n", A).

