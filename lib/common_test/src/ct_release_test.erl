%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2018. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% EXPERIMENTAL support for testing of upgrade.
%%
%% This is a library module containing support for test of release
%% related activities in one or more applications. Currenty it
%% supports upgrade only.
%%
%% == Configuration ==
%%
%% In order to find version numbers of applications to upgrade from,
%% ct_release_test needs to access and start old OTP
%% releases. A `common_test' configuration file can be used for
%% specifying the location of such releases, for example:
%%
%% ```
%% %% old-rels.cfg
%% {otp_releases,[{r16b,"/path/to/R16B03-1/bin/erl"},
%% 	       {'17',"/path/to/17.3/bin/erl"}]}.'''
%%
%% The configuration file should preferably point out the latest patch
%% level on each major release.
%%
%% If no such configuration file is given, init/1 will return
%% {skip,Reason} and any attempt at running upgrade/4
%% will fail.
%%
%% == Callback functions ==
%%
%% The following functions should be exported from a ct_release_test
%% callback module.
%%
%% All callback functions are called on the node where the upgrade is
%% executed.
%%
%%   Module:upgrade_init(CtData,State) -> NewState
%%   Types:
%%
%%     CtData = ct_data()
%%     State = NewState = cb_state()
%%
%%     Initialyze system before upgrade test starts.
%%
%%     This function is called before the upgrade is started. All
%%     applications given in upgrade/4 are already started by
%%     the boot script, so this callback is intended for additional
%%     initialization, if necessary.
%%
%%     CtData is an opaque data structure which shall be used
%%     in any call to ct_release_test inside the callback.
%%
%%     Example:
%%
%% upgrade_init(CtData,State) ->
%%     {ok,{FromVsn,ToVsn}} = ct_release_test:get_app_vsns(CtData,myapp),
%%     open_connection(State).
%%
%%   Module:upgrade_upgraded(CtData,State) -> NewState
%%   Types:
%%
%%     CtData = ct_data()
%%     State = NewState = cb_state()
%%
%%     Check that upgrade was successful.
%%
%%     This function is called after the release_handler has
%%     successfully unpacked and installed the new release, and it has
%%     been made permanent. It allows application specific checks to
%%     ensure that the upgrade was successful.
%%
%%     CtData is an opaque data structure which shall be used
%%     in any call to ct_release_test inside the callback.
%%
%%     Example:
%%
%% upgrade_upgraded(CtData,State) ->
%%     check_connection_still_open(State).
%%
%%   Module:upgrade_downgraded(CtData,State) -> NewState
%%   Types:
%%
%%     CtData = ct_data()
%%     State = NewState = cb_state()
%%
%%     Check that downgrade was successful.
%%
%%     This function is called after the release_handler has
%%     successfully re-installed the original release, and it has been
%%     made permanent. It allows application specific checks to ensure
%%     that the downgrade was successful.
%%
%%     CtData is an opaque data structure which shall be used
%%     in any call to ct_release_test inside the callback.
%%
%%     Example:
%%
%% upgrade_downgraded(CtData,State) ->
%%     check_connection_closed(State).
%%-----------------------------------------------------------------
-module(ct_release_test).

-export([init/1, upgrade/4, cleanup/1, get_app_vsns/2, get_appup/2]).

-include_lib("kernel/include/file.hrl").

%%-----------------------------------------------------------------
-define(testnode, 'ct_release_test-upgrade').
-define(exclude_apps, [hipe, dialyzer]). % never include these apps

%%-----------------------------------------------------------------
-record(ct_data, {from,to}).

%%-----------------------------------------------------------------
-type config() :: [{atom(),term()}].
-type cb_state() :: term().
-opaque ct_data() :: #ct_data{}.
-export_type([ct_data/0]).

-callback upgrade_init(ct_data(),cb_state()) -> cb_state().
-callback upgrade_upgraded(ct_data(),cb_state()) -> cb_state().
-callback upgrade_downgraded(ct_data(),cb_state()) -> cb_state().

%%-----------------------------------------------------------------
-spec init(Config) -> Result when
      Config :: config(),
      Result :: config() | SkipOrFail,
      SkipOrFail :: {skip,Reason} | {fail,Reason}.
%% Initialize ct_release_test.
%%
%% This function can be called from any of the
%% `init_per_*' functions in the test suite. It updates
%% the given `Config' with data that will be
%% used by future calls to other functions in this module. The
%% returned configuration must therefore also be returned from
%% the calling `init_per_*'.
%%
%% If the initialization fails, e.g. if a required release
%% cannot be found, the function returns `{skip,Reason}'. In
%% this case the other test support functions in this mudule
%% cannot be used.
%%
%% Example:
%%
%% init_per_suite(Config) ->
%%     ct_release_test:init(Config).
%%
init(Config) ->
    try init_upgrade_test() of
	{Major,Minor} ->
	    [{release_test,[{major,Major},{minor,Minor}]} | Config]
    catch throw:Thrown ->
	    Thrown
    end.

%%-----------------------------------------------------------------
-spec upgrade(App,Level,Callback,Config) -> any() when
      App :: atom(),
      Level :: minor | major,
      Callback :: {module(),InitState},
      InitState :: cb_state(),
      Config :: config();
	     (Apps,Level,Callback,Config) -> any() when
      Apps :: [App],
      App :: atom(),
      Level :: minor | major,
      Callback :: {module(),InitState},
      InitState :: cb_state(),
      Config :: config().
%% Test upgrade of the given application(s).
%%
%% This function can be called from a test case. It requires that
%% `Config' has been initialized by calling
%% init/1 prior to this, for example from `init_per_suite/1'.
%%
%% Upgrade tests are performed as follows:
%%
%%   Figure out which OTP release to test upgrade
%%     from. Start a node running that release and find the
%%     application versions on that node. Terminate the
%%     node.
%%   Figure out all dependencies for the applications under
%%     test.
%%   Create a release containing the core
%%     applications `kernel', `stdlib' and `sasl'
%%     in addition to the application(s) under test and all
%%     dependencies of these. The versions of the applications
%%     under test will be the ones found on the OTP release to
%%     upgrade from. The versions of all other applications will
%%     be those found on the current node, i.e. the common_test
%%     node. This is the "From"-release.
%%   Create another release containing the same
%%     applications as in the previous step, but with all
%%     application versions taken from the current node. This is
%%     the "To"-release.
%%   Install the "From"-release and start a new node
%%     running this release.
%%   Perform the upgrade test and allow customized
%%     control by using callbacks:
%%       Callback: `upgrade_init/2'
%%       Unpack the new release
%%       Install the new release
%%       Callback: `upgrade_upgraded/2'
%%       Install the original release
%%       Callback: `upgrade_downgraded/2'
%%
%% `App' or `Apps'
%% specifies the applications under test, i.e. the applications
%% which shall be upgraded. All other applications that are
%% included have the same releases in the "From"- and
%% "To"-releases and will therefore not be upgraded.
%%
%% `Level' specifies which OTP release to
%% pick the "From" versions from.
%%   major
%%   From verions are picked from the previous major
%%     release. For example, if the test is run on an OTP-17
%%     node, ct_release_test will pick the application
%%     "From" versions from an OTP installation running OTP
%%     R16B.
%%
%%   minor
%%   From verions are picked from the current major
%%     release. For example, if the test is run on an OTP-17
%%     node, ct_release_test will pick the application
%%     "From" versions from an OTP installation running an
%%     earlier patch level of OTP-17.
%%
%% The application "To" versions are allways picked from the
%% current node, i.e. the common_test node.
%%
%% `Callback' specifies the module (normally the
%% test suite) which implements the Callback functions, and
%% the initial value of the `State' variable used in these
%% functions.
%%
%% `Config' is the input argument received
%% in the test case function.
%%
%% Example:
%%
%% minor_upgrade(Config) ->
%%     ct_release_test:upgrade(ssl,minor,{?MODULE,[]},Config).
%%
upgrade(App,Level,Callback,Config) when is_atom(App) ->
    upgrade([App],Level,Callback,Config);
upgrade(Apps,Level,Callback,Config) ->
    Dir = proplists:get_value(priv_dir,Config),
    CreateDir = filename:join([Dir,Level,create]),
    InstallDir = filename:join([Dir,Level,install]),
    ok = filelib:ensure_dir(filename:join(CreateDir,"*")),
    ok = filelib:ensure_dir(filename:join(InstallDir,"*")),
    try upgrade(Apps,Level,Callback,CreateDir,InstallDir,Config) of
	ok ->
	    %%rm_rf(CreateDir),
	    Tars = filelib:wildcard(filename:join(CreateDir,"*.tar.gz")),
	    _ = [file:delete(Tar) || Tar <- Tars],
	    rm_rf(InstallDir),
	    ok
    catch throw:{fail,Reason} ->
	    ct:fail(Reason);
	  throw:{skip,Reason} ->
	    rm_rf(CreateDir),
	    rm_rf(InstallDir),
	    {skip,Reason}
    after
	%% Brutally kill all nodes that erroneously survived the test.
	%% Note, we will not reach this if the test fails with a
	%% timetrap timeout in the test suite! Thus we can have
	%% hanging nodes...
	Nodes = lists:filter(fun(Node) ->
				     case atom_to_list(Node) of
					 "ct_release_test-" ++_ -> true;
					 _ -> false
				     end
			     end,
			     nodes()),
	[rpc:call(Node,erlang,halt,[]) || Node <- Nodes]
    end.

%%-----------------------------------------------------------------
-spec cleanup(Config) -> Result when
      Config :: config(),
      Result :: config().
%% Clean up after tests.
%%
%% This function shall be called from the `end_per_*' function
%% complementing the `init_per_*' function where init/1
%% is called.
%%
%% It cleans up after the test, for example kills hanging
%% nodes.
%%
%% Example:
%%
%% end_per_suite(Config) ->
%%     ct_release_test:cleanup(Config).
%%
cleanup(Config) ->
    AllNodes = [node_name(?testnode)|nodes()],
    Nodes = lists:filter(fun(Node) ->
				 case atom_to_list(Node) of
				     "ct_release_test-" ++_ -> true;
				     _ -> false
				 end
			 end,
			 AllNodes),
    _ = [rpc:call(Node,erlang,halt,[]) || Node <- Nodes],
    Config.

%%-----------------------------------------------------------------
-spec get_app_vsns(CtData,App) -> {ok,{From,To}} | {error,Reason} when
      CtData :: ct_data(),
      App :: atom(),
      From :: string(),
      To :: string(),
      Reason :: {app_not_found,App}.
%% Get versions involved in this upgrade for the given application.
%%
%% This function can be called from inside any of the callback
%% functions. It returns the old (From) and new (To) versions involved
%% in the upgrade/downgrade test for the given application.
%%
%% CtData must be the first argument received in the
%% calling callback function - an opaque data structure set by
%% ct_release_tests.
get_app_vsns(#ct_data{from=FromApps,to=ToApps},App) ->
    case {lists:keyfind(App,1,FromApps),lists:keyfind(App,1,ToApps)} of
	{{App,FromVsn,_},{App,ToVsn,_}} ->
	    {ok,{FromVsn,ToVsn}};
	_ ->
	    {error,{app_not_found,App}}
    end.

%%-----------------------------------------------------------------
-spec get_appup(CtData,App) -> {ok,Appup} | {error,Reason} when
      CtData :: ct_data(),
      App :: atom(),
      Appup :: {From,To,Up,Down},
      From :: string(),
      To :: string(),
      Up :: [Instr],
      Down :: [Instr],
      Instr :: term(),
      Reason :: {app_not_found,App} | {vsn_not_found,{App,From}}.
%% Get appup instructions for the given application.
%%
%% This function can be called from inside any of the callback
%% functions. It reads the appup file for the given application and
%% returns the instructions for upgrade and downgrade for the versions
%% in the test.
%%
%% CtData must be the first argument received in the
%% calling callback function - an opaque data structure set by
%% ct_release_tests.
%%
%% See reference manual for appup files for types definitions for the
%% instructions.
get_appup(#ct_data{from=FromApps,to=ToApps},App) ->
    case lists:keyfind(App,1,ToApps) of
	{App,ToVsn,ToDir} ->
	    Appup = filename:join([ToDir, "ebin", atom_to_list(App)++".appup"]),
	    {ok, [{ToVsn, Ups, Downs}]} = file:consult(Appup),
	    {App,FromVsn,_} = lists:keyfind(App,1,FromApps),
	    case {systools_relup:appup_search_for_version(FromVsn,Ups),
		  systools_relup:appup_search_for_version(FromVsn,Downs)} of
		{{ok,Up},{ok,Down}} ->
		    {ok,{FromVsn,ToVsn,Up,Down}};
		_ ->
		    {error,{vsn_not_found,{App,FromVsn}}}
	    end;
	false ->
	    {error,{app_not_found,App}}
    end.

%%-----------------------------------------------------------------
init_upgrade_test() ->
    %% Check that a real release is running, not e.g. cerl
    ok = application:ensure_started(sasl),
    case release_handler:which_releases() of
	[{_,_,[],_}] ->
	    %% Fake release, no applications
	    throw({skip, "Need a real release running to create other releases"});
	_ ->
	    Major = init_upgrade_test(major),
	    Minor = init_upgrade_test(minor),
	    {Major,Minor}
    end.

init_upgrade_test(Level) ->
    {FromVsn,ToVsn} = get_rels(Level),
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
	    ct:log("Release ~tp is not available."
		   " Upgrade on '~p' level cannot be tested.",
		   [FromVsn,Level]),
	    undefined;
	_ ->
	    init_upgrade_test(FromVsn,ToVsn,OldRel)
    end.

get_rels(major) ->
    %% Given that the current major release is X, then this is an
    %% upgrade from major release X-1 to the current release.
    Current = erlang:system_info(otp_release),
    PreviousMajor = previous_major(Current),
    {PreviousMajor,Current};
get_rels(minor) ->
    %% Given that this is a (possibly) patched version of major
    %% release X, then this is an upgrade from major release X to the
    %% current release.
    CurrentMajor = erlang:system_info(otp_release),
    Current = CurrentMajor++"_patched",
    {CurrentMajor,Current}.

init_upgrade_test(FromVsn,ToVsn,OldRel) ->
    Name = list_to_atom("ct_release_test-otp-"++FromVsn),
    ct:log("Starting node to fetch application versions to upgrade from"),
    {ok,Node} = test_server:start_node(Name,peer,[{erl,[OldRel]}]),
    {Apps,Path} = fetch_all_apps(Node),
    test_server:stop_node(Node),
    {FromVsn,ToVsn,Apps,Path}.

fetch_all_apps(Node) ->
    Paths = rpc:call(Node,code,get_path,[]),
    %% Find all possible applications in the path
    AppFiles =
	lists:flatmap(
	  fun(P) ->
		  filelib:wildcard(filename:join(P,"*.app"))
	  end,
	  Paths),
    %% Figure out which version of each application is running on this
    %% node. Using application:load and application:get_key instead of
    %% reading the .app files since there might be multiple versions
    %% of a .app file and we only want the one that is actually
    %% running.
    AppVsns =
	lists:flatmap(
	  fun(F) ->
		  A = list_to_atom(filename:basename(filename:rootname(F))),
		  _ = rpc:call(Node,application,load,[A]),
		  case rpc:call(Node,application,get_key,[A,vsn]) of
		      {ok,V} -> [{A,V,rpc:call(Node,code,lib_dir,[A])}];
		      _ -> []
		  end
	  end,
	  AppFiles),
    ErtsVsn = rpc:call(Node, erlang, system_info, [version]),
    {[{erts,ErtsVsn}|AppVsns], Paths}.


%%-----------------------------------------------------------------
upgrade(Apps,Level,Callback,CreateDir,InstallDir,Config) ->
    ct:log("Test upgrade of the following applications: ~p",[Apps]),
    ct:log(".rel files and start scripts are created in:~n~ts",[CreateDir]),
    ct:log("The release is installed in:~n~ts",[InstallDir]),
    case proplists:get_value(release_test,Config) of
	undefined ->
	    throw({fail,"ct_release_test:init/1 not run"});
	RTConfig ->
	    case proplists:get_value(Level,RTConfig) of
		undefined ->
		    throw({skip,"Old release not available"});
		Data ->
		    {FromVsn,FromRel,FromAppsVsns} =
			target_system(Apps, CreateDir, InstallDir, Data),
		    {ToVsn,ToRel,ToAppsVsns} =
			upgrade_system(Apps, FromRel, CreateDir,
				       InstallDir, Data),
		    ct:log("Upgrade from: OTP-~ts, ~tp",[FromVsn, FromAppsVsns]),
		    ct:log("Upgrade to: OTP-~ts, ~tp",[ToVsn, ToAppsVsns]),
		    do_upgrade(Callback, FromVsn, FromAppsVsns, ToRel,
			       ToAppsVsns, InstallDir)
	    end
    end.

%%% This is similar to sasl/examples/src/target_system.erl, but with
%%% the following adjustments:
%%% - add a log directory
%%% - use an own 'start' script
%%% - chmod 'start' and 'start_erl'
target_system(Apps,CreateDir,InstallDir,{FromVsn,_,AllAppsVsns,Path}) ->
    RelName0 = "otp-"++FromVsn,

    AppsVsns = [{A,V,D} || {A,V,D} <- AllAppsVsns, lists:member(A,Apps)],
    {RelName,ErtsVsn} = create_relfile(AppsVsns,CreateDir,RelName0,FromVsn),

    %% Create .script and .boot
    ok = systools(make_script,[RelName,[{path,Path}]]),

    %% Create base tar file - i.e. erts and all apps
    ok = systools(make_tar,[RelName,[{erts,code:root_dir()},
				     {path,Path}]]),

    %% Unpack the tar to complete the installation
    ok = erl_tar:extract(RelName ++ ".tar.gz", [{cwd, InstallDir}, compressed]),

    %% Add bin and log dirs
    BinDir = filename:join([InstallDir, "bin"]),
    ok = make_dir(BinDir),
    ok = make_dir(filename:join(InstallDir,"log")),

    %% Delete start scripts - they will be added later
    ErtsBinDir = filename:join([InstallDir, "erts-" ++ ErtsVsn, "bin"]),
    ok = delete_file(filename:join([ErtsBinDir, "erl"])),
    ok = delete_file(filename:join([ErtsBinDir, "start"])),
    ok = delete_file(filename:join([ErtsBinDir, "start_erl"])),

    %% Copy .boot to bin/start.boot
    copy_file(RelName++".boot",filename:join([BinDir, "start.boot"])),

    %% Copy scripts from erts-xxx/bin to bin
    copy_file(filename:join([ErtsBinDir, "epmd"]),
              filename:join([BinDir, "epmd"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "run_erl"]),
              filename:join([BinDir, "run_erl"]), [preserve]),
    copy_file(filename:join([ErtsBinDir, "to_erl"]),
              filename:join([BinDir, "to_erl"]), [preserve]),

    %% create start_erl.data, sys.config and start.src
    StartErlData = filename:join([InstallDir, "releases", "start_erl.data"]),
    ok = write_file(StartErlData, io_lib:fwrite("~s ~s~n", [ErtsVsn, FromVsn])),
    SysConfig = filename:join([InstallDir, "releases", FromVsn, "sys.config"]),
    ok = write_file(SysConfig, "[]."),
    StartSrc = filename:join(ErtsBinDir,"start.src"),
    ok = write_file(StartSrc,start_script()),
    ok = file:change_mode(StartSrc,8#0755),

    %% Make start_erl executable
    %% (this has been fixed in OTP 17 - it is now installed with
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

    {FromVsn, RelName,AppsVsns}.

systools(Func,Args) ->
    case apply(systools,Func,Args) of
	ok ->
	    ok;
	error ->
	    throw({fail,{systools,Func,Args}})
    end.

%%% This is a copy of $ROOT/erts-xxx/bin/start.src, modified to add
%%% sname and heart
start_script() ->
    ["#!/bin/sh\n"
     "ROOTDIR=%FINAL_ROOTDIR%\n"
     "\n"
     "if [ -z \"$RELDIR\" ]\n"
     "then\n"
     "   RELDIR=$ROOTDIR/releases\n"
     "fi\n"
     "\n"
     "START_ERL_DATA=${1:-$RELDIR/start_erl.data}\n"
     "\n"
     "$ROOTDIR/bin/run_erl -daemon /tmp/ $ROOTDIR/log \"exec $ROOTDIR/bin/start_erl $ROOTDIR $RELDIR $START_ERL_DATA -sname ",atom_to_list(?testnode)," -heart\"\n"].

%%% Create a release containing the current (the test node) OTP
%%% release, including relup to allow upgrade from an earlier OTP
%%% release.
upgrade_system(Apps, FromRel, CreateDir, InstallDir, {_,ToVsn,_,_}) ->
    ct:log("Generating release to upgrade to."),

    RelName0 = "otp-"++ToVsn,

    AppsVsns = get_vsns(Apps),
    {RelName,_} = create_relfile(AppsVsns,CreateDir,RelName0,ToVsn),
    FromPath = filename:join([InstallDir,lib,"*",ebin]),

    ok = systools(make_script,[RelName]),
    ok = systools(make_relup,[RelName,[FromRel],[FromRel],
			      [{path,[FromPath]},
			       {outdir,CreateDir}]]),
    SysConfig = filename:join([CreateDir, "sys.config"]),
    ok = write_file(SysConfig, "[]."),

    ok = systools(make_tar,[RelName,[{erts,code:root_dir()}]]),

    {ToVsn, RelName,AppsVsns}.

%%% Start a new node running the release from target_system/6
%%% above. Then upgrade to the system from upgrade_system/6.
do_upgrade({Cb,InitState},FromVsn,FromAppsVsns,ToRel,ToAppsVsns,InstallDir) ->
    ct:log("Upgrade test attempting to start node.~n"
	   "If test fails, logs can be found in:~n~ts",
	   [filename:join(InstallDir,log)]),
    Start = filename:join([InstallDir,bin,start]),
    {ok,Node} = start_node(Start,FromVsn,FromAppsVsns),

    ct:log("Node started: ~p",[Node]),
    CtData = #ct_data{from = FromAppsVsns,to=ToAppsVsns},
    State1 = do_callback(Node,Cb,upgrade_init,[CtData,InitState]),

    [{"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    ToRelName = filename:basename(ToRel),
    copy_file(ToRel++".tar.gz",
	      filename:join([InstallDir,releases,ToRelName++".tar.gz"])),
    ct:log("Unpacking new release"),
    {ok,ToVsn} = rpc:call(Node,release_handler,unpack_release,[ToRelName]),
    [{"OTP upgrade test",ToVsn,_,unpacked},
     {"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    ct:log("Installing new release"),
    case rpc:call(Node,release_handler,install_release,[ToVsn]) of
	{ok,FromVsn,_} ->
	    ok;
	{continue_after_restart,FromVsn,_} ->
	    ct:log("Waiting for node restart")
    end,
    %% even if install_release returned {ok,...} there might be an
    %% emulator restart (instruction restart_emulator), so we must
    %% always make sure the node is running.
    {ok, _} = wait_node_up(current,ToVsn,ToAppsVsns),

    [{"OTP upgrade test",ToVsn,_,current},
     {"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),
    ct:log("Permanenting new release"),
    ok = rpc:call(Node,release_handler,make_permanent,[ToVsn]),
    [{"OTP upgrade test",ToVsn,_,permanent},
     {"OTP upgrade test",FromVsn,_,old}] =
	rpc:call(Node,release_handler,which_releases,[]),

    State2 = do_callback(Node,Cb,upgrade_upgraded,[CtData,State1]),

    ct:log("Re-installing old release"),
    case rpc:call(Node,release_handler,install_release,[FromVsn]) of
	{ok,FromVsn,_} ->
	    ok;
	{continue_after_restart,FromVsn,_} ->
	    ct:log("Waiting for node restart")
    end,
    %% even if install_release returned {ok,...} there might be an
    %% emulator restart (instruction restart_emulator), so we must
    %% always make sure the node is running.
    {ok, _} = wait_node_up(current,FromVsn,FromAppsVsns),

    [{"OTP upgrade test",ToVsn,_,permanent},
     {"OTP upgrade test",FromVsn,_,current}] =
	rpc:call(Node,release_handler,which_releases,[]),
    ct:log("Permanenting old release"),
    ok = rpc:call(Node,release_handler,make_permanent,[FromVsn]),
    [{"OTP upgrade test",ToVsn,_,old},
     {"OTP upgrade test",FromVsn,_,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),

    _State3 = do_callback(Node,Cb,upgrade_downgraded,[CtData,State2]),

    ct:log("Terminating node ~p",[Node]),
    erlang:monitor_node(Node,true),
    _ = rpc:call(Node,init,stop,[]),
    receive {nodedown,Node} -> ok end,
    ct:log("Node terminated"),

    ok.

do_callback(Node,Mod,Func,Args) ->
    Dir = filename:dirname(code:which(Mod)),
    _ = rpc:call(Node,code,add_path,[Dir]),
    ct:log("Calling ~p:~tp/1",[Mod,Func]),
    R = rpc:call(Node,Mod,Func,Args),
    ct:log("~p:~tp/~w returned: ~tp",[Mod,Func,length(Args),R]),
    case R of
	{badrpc,Error} ->
	    throw({fail,{test_upgrade_callback,Mod,Func,Args,Error}});
	NewState ->
	    NewState
    end.

%%% Library functions
previous_major("17") ->
    "r16b";
previous_major(Rel) ->
    integer_to_list(list_to_integer(Rel)-1).

create_relfile(AppsVsns,CreateDir,RelName0,RelVsn) ->
    UpgradeAppsVsns = [{A,V,restart_type(A)} || {A,V,_D} <- AppsVsns],

    CoreAppVsns0 = get_vsns([kernel,stdlib,sasl]),
    CoreAppVsns =
	[{A,V,restart_type(A)} || {A,V,_D} <- CoreAppVsns0,
				  false == lists:keymember(A,1,AppsVsns)],

    Apps = [App || {App,_,_} <- AppsVsns],
    StartDepsVsns = get_start_deps(Apps,CoreAppVsns),
    StartApps = [StartApp || {StartApp,_,_} <- StartDepsVsns] ++ Apps,

    {RuntimeDepsVsns,_} = get_runtime_deps(StartApps,StartApps,[],[]),

    AllAppsVsns0 = StartDepsVsns ++ UpgradeAppsVsns ++ RuntimeDepsVsns,

    %% Should test tools really be included? Some library functions
    %% here could be used by callback, but not everything since
    %% processes of these applications will not be running.
    TestToolAppsVsns0 = get_vsns([common_test]),
    TestToolAppsVsns =
	[{A,V,none} || {A,V,_D} <- TestToolAppsVsns0,
		       false == lists:keymember(A,1,AllAppsVsns0)],

    AllAppsVsns1 = AllAppsVsns0 ++ TestToolAppsVsns,
    AllAppsVsns = [AV || AV={A,_,_} <- AllAppsVsns1,
			 false == lists:member(A,?exclude_apps)],

    ErtsVsn = erlang:system_info(version),

    %% Create the .rel file
    RelContent = {release,{"OTP upgrade test",RelVsn},{erts,ErtsVsn},AllAppsVsns},
    RelName = filename:join(CreateDir,RelName0),
    RelFile = RelName++".rel",
    {ok,Fd} = file:open(RelFile,[write,{encoding,utf8}]),
    io:format(Fd,"~tp.~n",[RelContent]),
    ok = file:close(Fd),
    {RelName,ErtsVsn}.

get_vsns(Apps) ->
    [begin
	 _ = application:load(A),
	 {ok,V} = application:get_key(A,vsn),
	 {A,V,code:lib_dir(A)}
     end || A <- Apps].

get_start_deps([App|Apps],Acc) ->
    _ = application:load(App),
    {ok,StartDeps} = application:get_key(App,applications),
    StartDepsVsns =
	[begin
	     _ = application:load(StartApp),
	     {ok,StartVsn} = application:get_key(StartApp,vsn),
	     {StartApp,StartVsn,restart_type(StartApp)}
	 end || StartApp <- StartDeps,
		false == lists:keymember(StartApp,1,Acc)],
    DepsStartDeps = get_start_deps(StartDeps,Acc ++ StartDepsVsns),
    get_start_deps(Apps,DepsStartDeps);
get_start_deps([],Acc) ->
    Acc.

get_runtime_deps([App|Apps],StartApps,Acc,Visited) ->
    case lists:member(App,Visited) of
	true ->
	    get_runtime_deps(Apps,StartApps,Acc,Visited);
	false ->
	    %% runtime_dependencies should be possible to read with
	    %% application:get_key/2, but still isn't so we need to
	    %% read the .app file...
	    AppFile = code:where_is_file(atom_to_list(App) ++ ".app"),
	    {ok,[{application,App,Attrs}]} = file:consult(AppFile),
	    RuntimeDeps =
		lists:flatmap(
		  fun(Str) ->
			  [RuntimeAppStr,_] = string:lexemes(Str,"-"),
			  RuntimeApp = list_to_atom(RuntimeAppStr),
			  case {lists:keymember(RuntimeApp,1,Acc),
				lists:member(RuntimeApp,StartApps)} of
			      {false,false} when RuntimeApp=/=erts ->
				  [RuntimeApp];
			      _ ->
				  []
			  end
		  end,
		  proplists:get_value(runtime_dependencies,Attrs,[])),
	    RuntimeDepsVsns =
		[begin
		     _ = application:load(RuntimeApp),
		     {ok,RuntimeVsn} = application:get_key(RuntimeApp,vsn),
		     {RuntimeApp,RuntimeVsn,none}
		 end || RuntimeApp <- RuntimeDeps],
	    {DepsRuntimeDeps,NewVisited} =
		get_runtime_deps(RuntimeDeps,StartApps,Acc++RuntimeDepsVsns,[App|Visited]),
	    get_runtime_deps(Apps,StartApps,DepsRuntimeDeps,NewVisited)
    end;
get_runtime_deps([],_,Acc,Visited) ->
    {Acc,Visited}.

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
            ok = file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

write_file(FName, Conts) ->
    file:write_file(FName, unicode:characters_to_binary(Conts)).

%% Substitute all occurrences of %Var% for Val in the given scripts
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
    Conts = unicode:characters_to_list(Bin),
    NConts = subst(Conts, Vars),
    ok = write_file(Dest, NConts),
    case lists:member(preserve, Opts) of
        true ->
            {ok, FileInfo} = file:read_file_info(Src),
            file:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

subst(Str, [{Var,Val}|Vars]) ->
    subst(re:replace(Str,"%"++Var++"%",Val,[{return,list},unicode]),Vars);
subst(Str, []) ->
    Str.

%%% Start a node by executing the given start command. This node will
%%% be used for upgrade.
start_node(Start,ExpVsn,ExpAppsVsns) ->
    Port = open_port({spawn_executable, Start}, []),
    unlink(Port),
    erlang:port_close(Port),
    wait_node_up(permanent,ExpVsn,ExpAppsVsns).

wait_node_up(ExpStatus,ExpVsn,ExpAppsVsns0) ->
    Node = node_name(?testnode),
    ExpAppsVsns = [{A,V} || {A,V,_D} <- ExpAppsVsns0],
    wait_node_up(Node,ExpStatus,ExpVsn,lists:keysort(1,ExpAppsVsns),60).

wait_node_up(Node,ExpStatus,ExpVsn,ExpAppsVsns,0) ->
    test_server:fail({node_not_started,app_check_failed,ExpVsn,ExpAppsVsns,
		      rpc:call(Node,release_handler,which_releases,[ExpStatus]),
		      rpc:call(Node,application,which_applications,[])});
wait_node_up(Node,ExpStatus,ExpVsn,ExpAppsVsns,N) ->
    case {rpc:call(Node,release_handler,which_releases,[ExpStatus]),
	  rpc:call(Node, application, which_applications, [])} of
	{[{_,ExpVsn,_,_}],Apps} when is_list(Apps) ->
	    case [{A,V} || {A,_,V} <- lists:keysort(1,Apps),
                           lists:keymember(A,1,ExpAppsVsns)] of
		ExpAppsVsns ->
		    {ok,Node};
		_ ->
		    timer:sleep(2000),
		    wait_node_up(Node,ExpStatus,ExpVsn,ExpAppsVsns,N-1)
	    end;
	_ ->
	    timer:sleep(2000),
	    wait_node_up(Node,ExpStatus,ExpVsn,ExpAppsVsns,N-1)
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

delete_file(FileName) ->
    case file:delete(FileName) of
        {error, enoent} ->
            ok;
        Else ->
            Else
    end.

make_dir(Dir) ->
    case file:make_dir(Dir) of
        {error, eexist} ->
            ok;
        Else ->
            Else
    end.
