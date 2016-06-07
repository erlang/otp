%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(crashdump_viewer_SUITE).

-include_lib("observer/src/crashdump_viewer.hrl").

%% Test functions
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 start_stop/1,load_file/1,not_found_items/1,
	 non_existing/1,not_a_crashdump/1,old_crashdump/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(failed_file,"failed-cases.txt").
-define(helper_mod,crashdump_helper).



init_per_testcase(start_stop, Config) ->
    catch crashdump_viewer:stop(),
    try
	case os:type() of
	    {unix,darwin} ->
		exit("Can not test on MacOSX");
	    {unix, _} ->
		io:format("DISPLAY ~s~n", [os:getenv("DISPLAY")]),
		case ct:get_config(xserver, none) of
		    none -> ignore;
		    Server -> os:putenv("DISPLAY", Server)
		end;
	    _ -> ignore
	end,
	wx:new(),
	wx:destroy(),
	Config
    catch
	_:undef ->
	    {skipped, "No wx compiled for this platform"};
	_:Reason ->
	    SkipReason = io_lib:format("Start wx failed: ~p", [Reason]),
	    {skipped, lists:flatten(SkipReason)}
    end;
init_per_testcase(_Case, Config) ->
    catch crashdump_viewer:stop(),
    Config.
end_per_testcase(Case, Config) ->
    case ?config(tc_status,Config) of
	ok ->
	    ok;
	_Fail ->
	    File = filename:join(?config(data_dir,Config),?failed_file),
	    {ok,Fd}=file:open(File,[append]),
	    file:write(Fd,io_lib:format("~w.~n",[Case])),
	    file:close(Fd)
    end,
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_stop,
     non_existing,
     not_a_crashdump,
     old_crashdump,
     load_file,
     not_found_items
    ].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Create a lot of crashdumps which can be used in the testcases below
init_per_suite(Config) when is_list(Config) ->
    delete_saved(Config),
    DataDir = ?config(data_dir,Config),
    Rels = [R || R <- ['17','18'], ?t:is_release_available(R)] ++ [current],
    io:format("Creating crash dumps for the following releases: ~p", [Rels]),
    AllDumps = create_dumps(DataDir,Rels),
    [{dumps,AllDumps}|Config].

delete_saved(Config) ->
    DataDir = ?config(data_dir,Config),
    file:delete(filename:join(DataDir,?failed_file)),
    SaveDir = filename:join(DataDir,"save"),
    Dumps = filelib:wildcard(filename:join(SaveDir,"*")),
    lists:foreach(fun(F) -> file:delete(F) end, Dumps),
    file:del_dir(SaveDir),
    ok.


start_stop(Config) when is_list(Config) ->
    Dump = hd(?config(dumps,Config)),
    timer:sleep(1000),

    ProcsBefore = processes(),
    NumProcsBefore = length(ProcsBefore),
    ok = crashdump_viewer:start(Dump),
    ExpectedRegistered = [crashdump_viewer_server,
			  cdv_wx,
			  cdv_proc_cb,
			  cdv_proc_cb__holder,
			  cdv_port_cb,
			  cdv_port_cb__holder,
			  cdv_ets_cb,
			  cdv_ets_cb__holder,
			  cdv_timer_cb,
			  cdv_timer_cb__holder,
			  cdv_fun_cb,
			  cdv_fun_cb__holder,
			  cdv_atom_cb,
			  cdv_atom_cb__holder,
			  cdv_dist_cb,
			  cdv_dist_cb__holder,
			  cdv_mod_cb,
			  cdv_mod_cb__holder],
    Regs=[begin
	      P=whereis(N),
	      {P,N,erlang:monitor(process,P)}
	  end || N <- ExpectedRegistered],
    ct:log("CDV procs: ~n~p~n",[Regs]),
    [true=is_pid(P) || {P,_,_} <- Regs],
    timer:sleep(5000), % give some time to live
    ok = crashdump_viewer:stop(),
    recv_downs(Regs),
    timer:sleep(2000),
    ProcsAfter = processes(),
    NumProcsAfter = length(ProcsAfter),
    if NumProcsAfter=/=NumProcsBefore ->
	    ct:log("Before but not after:~n~p~n",
		   [[{P,process_info(P)} || P <- ProcsBefore -- ProcsAfter]]),
	    ct:log("After but not before:~n~p~n",
		   [[{P,process_info(P)} || P <- ProcsAfter -- ProcsBefore]]),
	    ct:fail("leaking processes");
       true ->
	    ok
    end,
    ok.

recv_downs([]) ->
    ok;
recv_downs(Regs) ->
    receive
	{'DOWN',Ref,process,_Pid,_} ->
	    ct:log("Got 'DOWN' for process ~n~p~n",[_Pid]),
	    recv_downs(lists:keydelete(Ref,3,Regs))
    after 30000 ->
	    ct:log("Timeout waiting for down:~n~p~n",
		   [[{Reg,process_info(P)} || {P,_,_}=Reg <- Regs]]),
	    ct:log("Message queue:~n~p~n",[process_info(self(),messages)])
    end.

%% Try to load nonexisting file
non_existing(Config) when is_list(Config) ->
    ExpectedReason = "non-existing-file is not an Erlang crash dump\n",
    {error, ExpectedReason} = start_backend("non-existing-file"),
    ok = crashdump_viewer:stop().

%% Try to load a crashdump of old (earlier than OTP R10B) format
old_crashdump(Config) when is_list(Config) ->
    DataDir = ?config(data_dir,Config),
    OldFile = filename:join(DataDir,"old_format.dump"),
    ExpectedReason = "The crashdump " ++ OldFile ++
	" is in the pre-R10B format, which is no longer supported.\n",
    {error, ExpectedReason} = start_backend(OldFile),
    ok = crashdump_viewer:stop().

%% Try to load a file which is not an erlang crashdump
not_a_crashdump(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir,Config),
    F1 = filename:join(PrivDir,"f1"),
    F2 = filename:join(PrivDir,"f2"),

    file:write_file(F1,"=unexpected_tag:xyz"),
    file:write_file(F2,""),

    ExpReason1 = F1 ++ " is not an Erlang crash dump\n",
    ExpReason2 = F2 ++ " is not an Erlang crash dump\n",

    {error,ExpReason1} = start_backend(F1),
    {error,ExpReason2} = start_backend(F2),

    ok = crashdump_viewer:stop().

%% Load files into the tool and view all pages
load_file(Config) when is_list(Config) ->
    case ?t:is_debug() of
	true ->
	    {skip,"Debug-compiled emulator -- far too slow"};
	false ->
	    load_file_1(Config)
    end.


load_file_1(Config) ->
    DataDir = ?config(data_dir,Config),
    crashdump_viewer:start_link(),

    %% Read both created and predefined dumps
    AllFiles = filelib:wildcard(filename:join(DataDir,"r*_dump.*")),
    lists:foreach(
      fun(File) ->
	      Content = browse_file(File),
	      special(File,Content)
      end,
      AllFiles),
    ok = crashdump_viewer:stop().

%% Try to lookup nonexisting process, port and node
not_found_items(Config) ->
    Dump = hd(?config(dumps,Config)),

    ok = start_backend(Dump),

    {ok,#general_info{},_} = crashdump_viewer:general_info(),

    {error,not_found} = crashdump_viewer:proc_details("<1111.1111.1111>"),
    {error,not_found} = crashdump_viewer:port("#Port<1111.1111>"),
    {error,not_found} = crashdump_viewer:node_info("1111"),

    ok = crashdump_viewer:stop().

%% Remove generated crashdumps
end_per_suite(Config) when is_list(Config) ->
    Dumps = ?config(dumps,Config),
    DataDir = ?config(data_dir,Config),
    FailedFile = filename:join(DataDir,?failed_file),
    case filelib:is_file(FailedFile) of
	true ->
	    SaveDir = filename:join(DataDir,"save"),
	    file:make_dir(SaveDir),
	    file:copy(FailedFile,filename:join(SaveDir,?failed_file)),
	    lists:foreach(
	      fun(CD) ->
		      File = filename:basename(CD),
		      New = filename:join(SaveDir,File),
		      file:copy(CD,New)
	      end, Dumps);
	false ->
	    ok
    end,
    file:delete(FailedFile),
    lists:foreach(fun(CD) -> ok = file:delete(CD) end,Dumps),
    lists:keydelete(dumps,1,Config).


%%%-----------------------------------------------------------------
%%% Internal
%%%-----------------------------------------------------------------
%%% Start the crashdump_viewer backend and load a dump
start_backend(File) ->
    crashdump_viewer:start_link(),
    register_progress_handler(),
    ok = crashdump_viewer:read_file(File),
    wait_for_progress_done().

%%%-----------------------------------------------------------------
%%% Simulate the progress handler in observer_lib
register_progress_handler() ->
    register(cdv_progress_handler,self()).

wait_for_progress_done() ->
    receive
	{progress,{error,Reason}} ->
	    unregister(cdv_progress_handler),
	    {error,lists:flatten(Reason)};
	{progress,{ok,done}} ->
	    unregister(cdv_progress_handler),
	    ok;
	{progress,_} ->
	    wait_for_progress_done()
    end.

%%%-----------------------------------------------------------------
%%% General check of what is displayed for a dump
browse_file(File) ->
    io:format("~nBrowsing file: ~s",[File]),

    ok = start_backend(File),

    io:format("  backend started",[]),

    {ok,_GI=#general_info{},_GenTW} = crashdump_viewer:general_info(),
    {ok,Procs,_ProcsTW} = crashdump_viewer:processes(),
    {ok,Ports,_PortsTW} = crashdump_viewer:ports(),
    {ok,_Ets,_EtsTW} = crashdump_viewer:ets_tables(all),
    {ok,_IntEts,_IntEtsTW} = crashdump_viewer:internal_ets_tables(),
    {ok,_Timers,_TimersTW} = crashdump_viewer:timers(all),
    {ok,_Funs,_FunsTW} = crashdump_viewer:funs(),
    {ok,_Atoms,_AtomsTW} = crashdump_viewer:atoms(),
    {ok,Nodes,_NodesTW} = crashdump_viewer:dist_info(),
    {ok,Mods,_ModsTW} = crashdump_viewer:loaded_modules(),
    {ok,_Mem,_MemTW} = crashdump_viewer:memory(),
    {ok,_AllocAreas,_AreaTW} = crashdump_viewer:allocated_areas(),
    {ok,_AllocINfo,_AllocInfoTW} = crashdump_viewer:allocator_info(),
    {ok,_HashTabs,_HashTabsTW} = crashdump_viewer:hash_tables(),
    {ok,_IndexTabs,_IndexTabsTW} = crashdump_viewer:index_tables(),

    io:format("  info read",[]),

    lookat_all_pids(Procs),
    io:format("  pids ok",[]),
    lookat_all_ports(Ports),
    io:format("  ports ok",[]),
    lookat_all_mods(Mods),
    io:format("  mods ok",[]),
    lookat_all_nodes(Nodes),
    io:format("  nodes ok",[]),

    Procs. % used as second arg to special/2

special(File,Procs) ->
    case filename:extension(File) of
	".full_dist" ->
	    %% I registered a process as aaaaaaaa in the full_dist dumps 
	    %% to make sure it will be the first in the list when sorted
	    %% on names. There are some special data here, so I'll thoroughly
	    %% read the process details for this process. Other processes
	    %% are just briefly traversed.
	    [#proc{pid=Pid0}|_Rest] = lists:keysort(#proc.name,Procs),
	    Pid = pid_to_list(Pid0),
	    {ok,ProcDetails=#proc{},[]} = crashdump_viewer:proc_details(Pid),
	    io:format("  process details ok",[]),

	    #proc{dict=Dict} = ProcDetails,

	    ['#CDVBin',Offset,Size,Pos] = proplists:get_value(bin,Dict),
	    {ok,<<_:Size/binary>>} =
		crashdump_viewer:expand_binary({Offset,Size,Pos}),
	    {ok,'#CDVTruncatedBinary'} =
		crashdump_viewer:expand_binary({Offset,Size+1,Pos}),
	    ['#CDVBin',SOffset,SSize,SPos] = proplists:get_value(sub_bin,Dict),
	    {ok,<<_:SSize/binary>>} =
		crashdump_viewer:expand_binary({SOffset,SSize,SPos}),
	    io:format("  expand binary ok",[]),

	    ['#CDVPid',X1,Y1,Z1] = proplists:get_value(ext_pid,Dict),
	    ChannelStr1 = integer_to_list(X1),
	    ExtPid =
		"<" ++ ChannelStr1 ++ "." ++
		integer_to_list(Y1) ++ "." ++
		integer_to_list(Z1) ++ ">",
	    {error,{other_node,ChannelStr1}} =
		crashdump_viewer:proc_details(ExtPid),
	    io:format("  process details external ok",[]),

	    ['#CDVPort',X2,Y2] = proplists:get_value(port,Dict),
	    ChannelStr2 = integer_to_list(X2),
	    Port = "#Port<"++ChannelStr2++"."++integer_to_list(Y2)++">",
	    {ok,_PortDetails=#port{},[]} = crashdump_viewer:port(Port),
	    io:format("  port details ok",[]),

	    ['#CDVPort',X3,Y3] = proplists:get_value(ext_port,Dict),
	    ChannelStr3 = integer_to_list(X3),
	    ExtPort = "#Port<"++ChannelStr3++"."++integer_to_list(Y3)++">",
	    {error,{other_node,ChannelStr3}} = crashdump_viewer:port(ExtPort),
	    io:format("  port details external ok",[]),

	    {ok,[_Ets=#ets_table{}],[]} = crashdump_viewer:ets_tables(Pid),
	    io:format("  ets tables ok",[]),

	    {ok,[#timer{pid=Pid0,name=undefined},
		 #timer{pid=Pid0,name="aaaaaaaa"}],[]} =
		crashdump_viewer:timers(Pid),
	    {ok,AllTimers,_TimersTW} = crashdump_viewer:timers(all),
	    #timer{name="noexistproc"} =
		lists:keyfind(undefined,#timer.pid,AllTimers),
	    io:format("  timers ok:",[]),

	    {ok,Mod1=#loaded_mod{},[]} =
		crashdump_viewer:loaded_mod_details(atom_to_list(?helper_mod)),
	    io:format("  modules ok",[]),
	    #loaded_mod{current_size=CS, old_size=OS,
			old_attrib=A,old_comp_info=C}=Mod1,
	    true = is_integer(CS),
	    true = (CS==OS),
	    true = (A=/=undefined),
	    true = (C=/=undefined),
	    {ok,Mod2=#loaded_mod{},[]} =
		crashdump_viewer:loaded_mod_details("application"),
	    io:format("  module details ok",[]),
	    #loaded_mod{old_size="No old code exists",
			old_attrib=undefined,
			old_comp_info=undefined}=Mod2,
	    ok;
	%% ".strangemodname" ->
	%%     {ok,Mods,[]} = crashdump_viewer:loaded_modules(),
	%%     lookat_all_mods(Mods),
	%%     ok;
	%% ".sort" ->
	%%     %% sort ports, atoms and modules ????
	%%     ok;
	%% ".trunc" ->
	%%     %% ????
	%%     ok;
	_ ->
	    ok
    end,
    ok.


lookat_all_pids([]) ->
    ok;
lookat_all_pids([#proc{pid=Pid0}|Procs]) ->
    Pid = pid_to_list(Pid0),
    {ok,_ProcDetails=#proc{},_ProcTW} = crashdump_viewer:proc_details(Pid),
    {ok,_Ets,_EtsTW} = crashdump_viewer:ets_tables(Pid),
    {ok,_Timers,_TimersTW} = crashdump_viewer:timers(Pid),
    lookat_all_pids(Procs).

lookat_all_ports([]) ->
    ok;
lookat_all_ports([#port{id=Port0}|Procs]) ->
    Port = cdv_port_cb:format(Port0),
    {ok,_PortDetails=#port{},_PortTW} = crashdump_viewer:port(Port),
    lookat_all_ports(Procs).

lookat_all_mods([]) ->
    ok;
lookat_all_mods([#loaded_mod{mod=ModId}|Mods]) ->
    ModName = cdv_mod_cb:format(ModId),
    {ok,_Mod=#loaded_mod{},_ModTW} = crashdump_viewer:loaded_mod_details(ModName),
    lookat_all_mods(Mods).

lookat_all_nodes([]) ->
    ok;
lookat_all_nodes([#nod{channel=Channel0}|Nodes]) ->
    Channel = integer_to_list(Channel0),
    {ok,_Node=#nod{},_NodeTW} = crashdump_viewer:node_info(Channel),
    lookat_all_nodes(Nodes).

%%%-----------------------------------------------------------------
%%% 
create_dumps(DataDir,Rels) ->
    create_dumps(DataDir,Rels,[]).
create_dumps(DataDir,[Rel|Rels],Acc) ->
    Fun = fun() -> do_create_dumps(DataDir,Rel) end,
    Pa = filename:dirname(code:which(?MODULE)),
    {Dumps,DosDump} =
	?t:run_on_shielded_node(Fun, compat_rel(Rel) ++ "-pa \"" ++ Pa ++ "\""),
    create_dumps(DataDir,Rels,Dumps ++ Acc ++ DosDump);
create_dumps(_DataDir,[],Acc) ->
    Acc.

do_create_dumps(DataDir,Rel) ->
    CD1 = full_dist_dump(DataDir,Rel),
    CD2 = dump_with_args(DataDir,Rel,"port_is_unix_fd","-oldshell"),
    DosDump = 
	case os:type() of
	    {unix,sunos} -> dos_dump(DataDir,Rel,CD1);
	    _ -> []
	end,
    case Rel of
	current ->
	    CD3 = dump_with_args(DataDir,Rel,"instr","+Mim true"),
	    CD4 = dump_with_strange_module_name(DataDir,Rel,"strangemodname"),
	    {[CD1,CD2,CD3,CD4], DosDump};
	_ ->
	    {[CD1,CD2], DosDump}
    end.


%% Create a dump which has three visible nodes, one hidden and one
%% not connected node, and with monitors and links between nodes.
%% One of the visible nodes is stopped and started again in order to
%% get multiple creations.
full_dist_dump(DataDir,Rel) ->
    Opt = rel_opt(Rel),
    Pz = "-pz \"" ++ filename:dirname(code:which(?MODULE)) ++ "\"",
    PzOpt = [{args,Pz}],
    {ok,N1} = ?t:start_node(n1,peer,Opt ++ PzOpt),
    {ok,N2} = ?t:start_node(n2,peer,Opt ++ PzOpt),
    {ok,N3} = ?t:start_node(n3,peer,Opt ++ PzOpt),
    {ok,N4} = ?t:start_node(n4,peer,Opt ++ [{args,"-hidden " ++ Pz}]),
    Creator = self(),

    P1 = rpc:call(N1,?helper_mod,n1_proc,[N2,Creator]),
    P2 = rpc:call(N2,?helper_mod,remote_proc,[P1,Creator]),
    P3 = rpc:call(N3,?helper_mod,remote_proc,[P1,Creator]),
    P4 = rpc:call(N4,?helper_mod,remote_proc,[P1,Creator]),
    
    get_response(P2),
    get_response(P3),
    get_response(P4),
    get_response(P1),

    %% start, stop and start a node in order to get multiple 'creations'
    {ok,N5} = ?t:start_node(n5,peer,Opt ++ PzOpt),
    P51 = rpc:call(N5,?helper_mod,remote_proc,[P1,Creator]),
    get_response(P51),
    ?t:stop_node(N5),
    {ok,N5} = ?t:start_node(n5,peer,Opt ++ PzOpt),
    P52 = rpc:call(N5,?helper_mod,remote_proc,[P1,Creator]),
    get_response(P52),

    {aaaaaaaa,N1} ! {hello,from,other,node}, % distribution message
    
    ?t:stop_node(N3),
    DumpName = "full_dist",
    CD = dump(N1,DataDir,Rel,DumpName),

    ?t:stop_node(N2),
    ?t:stop_node(N4),
    ?t:stop_node(N5),
    CD.

get_response(P) ->
    receive {P,done} -> ok
    after 3000 -> ?t:fail({get_response_timeout,P,node(P)})
    end.


dump_with_args(DataDir,Rel,DumpName,Args) ->
    RelOpt = rel_opt(Rel),
    Opt = RelOpt ++ [{args,Args}],
    {ok,N1} = ?t:start_node(n1,peer,Opt),
    CD = dump(N1,DataDir,Rel,DumpName),
    ?t:stop_node(n1),
    CD.

%% This dump is added to test OTP-10090 - regarding URL encoding of
%% module names in the module detail link.
dump_with_strange_module_name(DataDir,Rel,DumpName) ->
    Opt = rel_opt(Rel),
    {ok,N1} = ?t:start_node(n1,peer,Opt),

    Mod = '<mod ule#with?strange%name>',
    File = atom_to_list(Mod) ++ ".erl",
    Forms = [{attribute,1,file,{File,1}},
	     {attribute,1,module,Mod},
	     {eof,4}],
    {ok,Mod,Bin} = rpc:call(N1,compile,forms,[Forms,[binary]]),
    {module,Mod} = rpc:call(N1,code,load_binary,[Mod,File,Bin]),
    CD = dump(N1,DataDir,Rel,DumpName),
    ?t:stop_node(n1),
    CD.

dump(Node,DataDir,Rel,DumpName) ->
    Crashdump = filename:join(DataDir, dump_prefix(Rel)++DumpName),
    rpc:call(Node,os,putenv,["ERL_CRASH_DUMP",Crashdump]),
    rpc:call(Node,erlang,halt,[DumpName]),
    ok = check_complete(Crashdump),
    Crashdump.

check_complete(File) ->
    check_complete1(File,10).

check_complete1(_File,0) ->
    {error,enoent};
check_complete1(File,N) ->
    case file:read_file_info(File) of
	{error,enoent} ->
	    timer:sleep(500),
	    check_complete1(File,N-1);
	{ok,#file_info{size=Size}} ->
	    check_complete2(File,Size)
    end.

check_complete2(File,Size) ->
    timer:sleep(500),
    case file:read_file_info(File) of
	{ok,#file_info{size=Size}} ->
	    ok;
	{ok,#file_info{size=OtherSize}} ->
	    check_complete2(File,OtherSize)
    end.

dos_dump(DataDir,Rel,Dump) ->
    DosDumpName = filename:join(DataDir,dump_prefix(Rel)++"dos"),
    Cmd = "unix2dos " ++ Dump ++ " > " ++ DosDumpName,
    Port = open_port({spawn,Cmd},[exit_status]),
    receive
	{Port,{exit_status,0}} -> 
	    [DosDumpName];
	{Port,{exit_status,_Error}} ->
	    ?t:comment("Couldn't run \'unix2dos\'"),
	    []
    end.

rel_opt(Rel) ->
    case Rel of
	'17' -> [{erl,[{release,"17_latest"}]}];
	'18' -> [{erl,[{release,"18_latest"}]}];
	current -> []
    end.

dump_prefix(Rel) ->
    case Rel of
	'17' -> "r17_dump.";
	'18' -> "r18_dump.";
	current -> "r19_dump."
    end.

compat_rel(Rel) ->
    case Rel of
	'17' -> "+R17 ";
	'18' -> "+R18 ";
	current -> ""
    end.
