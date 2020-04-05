%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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
	 non_existing/1,not_a_crashdump/1,old_crashdump/1,new_crashdump/1]).
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

suite() -> [].

all() -> 
    [start_stop,
     non_existing,
     not_a_crashdump,
     old_crashdump,
     new_crashdump,
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
    CurrVsn = list_to_integer(erlang:system_info(otp_release)),
    OldRels = [R || R <- [CurrVsn-2,CurrVsn-1],
		    ?t:is_release_available(list_to_atom(integer_to_list(R)))],
    Rels = OldRels ++ [current],
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

%% Try to load a file with newer version than this crashdump viewer can handle
new_crashdump(Config) ->
    Dump = hd(?config(dumps,Config)),
    ok = start_backend(Dump),
    {ok,{MaxVsn,CurrentVsn}} = crashdump_viewer:get_dump_versions(),
    if MaxVsn =/= CurrentVsn ->
            ct:fail("Current dump version is not equal to cdv's max version");
       true ->
            ok
    end,
    ok = crashdump_viewer:stop(),
    NewerVsn = lists:join($.,[integer_to_list(X+1) || X <- MaxVsn]),
    PrivDir = ?config(priv_dir,Config),
    NewDump = filename:join(PrivDir,"new_erl_crash.dump"),
    ok = file:write_file(NewDump,"=erl_crash_dump:"++NewerVsn++"\n"),
    {error, Reason} = start_backend(NewDump),
    "This Crashdump Viewer is too old" ++_ = Reason,
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
    {ok,_PTs,_PTsTW} = crashdump_viewer:persistent_terms(),

    io:format("  info read",[]),

    lookat_all_pids(Procs,is_truncated(File),incomplete_allowed(File)),
    io:format("  pids ok",[]),
    lookat_all_ports(Ports),
    io:format("  ports ok",[]),
    lookat_all_mods(Mods),
    io:format("  mods ok",[]),
    lookat_all_nodes(Nodes),
    io:format("  nodes ok",[]),

    Procs. % used as second arg to special/2

is_truncated(File) ->
    case filename:extension(File) of
        ".trunc"++_ ->
            true;
        _ ->
            false
    end.

incomplete_allowed(File) ->
    %% Incomplete heap is allowed for native libs, since some literals
    %% are not dumped - and for pre OTP-20 (really pre 20.2) releases,
    %% since literals were not dumped at all then.
    Rel = get_rel_from_dump_name(File),
    Rel < 20 orelse test_server:is_native(lists).

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

            ProcBins = proplists:get_value(proc_bins,Dict),
            {['#CDVBin',0,65,ProcBin],
             ['#CDVBin',65,65,ProcBin],
             ['#CDVBin',130,125,ProcBin]} = ProcBins,
            io:format("  ProcBins ok",[]),


            Binaries = crashdump_helper:create_binaries(),
            verify_binaries(Binaries, proplists:get_value(bins,Dict)),
	    io:format("  binaries ok",[]),

            SubBinaries = crashdump_helper:create_sub_binaries(Binaries),
            verify_binaries(SubBinaries, proplists:get_value(sub_bins,Dict)),
	    io:format("  sub binaries ok",[]),

	    #proc{last_calls=LastCalls} = ProcDetails,
            true = length(LastCalls) =< 4,

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
        ".trunc_mod" ->
            ModName = atom_to_list(?helper_mod),
            {ok,Mod=#loaded_mod{},[TW]} =
                crashdump_viewer:loaded_mod_details(ModName),
            "WARNING: The crash dump is truncated here."++_ = TW,
            #loaded_mod{current_attrib=CA,current_comp_info=CCI,
                        old_attrib=OA,old_comp_info=OCI} = Mod,
            case lists:all(fun(undefined) ->
                                   true;
                              (S) when is_list(S) ->
                                   io_lib:printable_unicode_list(lists:flatten(S));
                              (_) -> false
                           end,
                           [CA,CCI,OA,OCI]) of
                true ->
                    ok;
                false ->
                    ct:fail({should_be_printable_strings_or_undefined,
                             {CA,CCI,OA,OCI}})
            end,
            ok;
	".trunc_bin1" ->
            %% This is 'full_dist' truncated after the first
            %% "=binary:"
            %% i.e. no binary exist in the dump
	    [#proc{pid=Pid0}|_Rest] = lists:keysort(#proc.name,Procs),
	    Pid = pid_to_list(Pid0),
            %%WarnIncompleteHeap = ["WARNING: This process has an incomplete heap. Some information might be missing."],
	    {ok,ProcDetails=#proc{},[]} =
                crashdump_viewer:proc_details(Pid),
	    io:format("  process details ok",[]),

	    #proc{dict=Dict} = ProcDetails,

	    '#CDVNonexistingBinary' = proplists:get_value(bin,Dict),
	    '#CDVNonexistingBinary' = proplists:get_value(sub_bin,Dict),

	    io:format("  nonexisting binaries ok",[]),
            ok;
	".trunc_bin2" ->
            %% This is 'full_dist' truncated after the first
            %% "=binary:Addr\n
            %%  Size"
            %% i.e. binaries are truncated
	    [#proc{pid=Pid0}|_Rest] = lists:keysort(#proc.name,Procs),
	    Pid = pid_to_list(Pid0),
	    {ok,ProcDetails=#proc{},[]} = crashdump_viewer:proc_details(Pid),
	    io:format("  process details ok",[]),

	    #proc{dict=Dict} = ProcDetails,

	    ['#CDVBin',Offset,Size,Pos] = proplists:get_value(bin,Dict),
            {ok,'#CDVTruncatedBinary'} =
		crashdump_viewer:expand_binary({Offset,Size,Pos}),
	    ['#CDVBin',SOffset,SSize,SPos] = proplists:get_value(sub_bin,Dict),
            {ok,'#CDVTruncatedBinary'} =
		crashdump_viewer:expand_binary({SOffset,SSize,SPos}),

	    io:format("  expand truncated binary ok",[]),
            ok;
	".trunc_bin3" ->
            %% This is 'full_dist' truncated after the first
            %% "=binary:Addr\n
            %%  Size:"
            %% i.e. same as 'trunc_bin2', except the colon exists also
	    [#proc{pid=Pid0}|_Rest] = lists:keysort(#proc.name,Procs),
	    Pid = pid_to_list(Pid0),
	    {ok,ProcDetails=#proc{},[]} = crashdump_viewer:proc_details(Pid),
	    io:format("  process details ok",[]),

	    #proc{dict=Dict} = ProcDetails,

	    ['#CDVBin',Offset,Size,Pos] = proplists:get_value(bin,Dict),
            {ok,'#CDVTruncatedBinary'} =
		crashdump_viewer:expand_binary({Offset,Size,Pos}),
	    ['#CDVBin',SOffset,SSize,SPos] = proplists:get_value(sub_bin,Dict),
            {ok,'#CDVTruncatedBinary'} =
		crashdump_viewer:expand_binary({SOffset,SSize,SPos}),

	    io:format("  expand truncated binary ok",[]),
            ok;
	".trunc_bin4" ->
            %% This is 'full_dist' truncated after the first
            %% "=binary:Addr\n
            %%  Size:BinaryMissinOneByte"
            %% i.e. the full binary is truncated, but the sub binary is complete
	    [#proc{pid=Pid0}|_Rest] = lists:keysort(#proc.name,Procs),
	    Pid = pid_to_list(Pid0),
	    {ok,ProcDetails=#proc{},[]} = crashdump_viewer:proc_details(Pid),
	    io:format("  process details ok",[]),

	    #proc{dict=Dict} = ProcDetails,

	    ['#CDVBin',Offset,Size,Pos] = proplists:get_value(bin,Dict),
            {ok,'#CDVTruncatedBinary'} =
		crashdump_viewer:expand_binary({Offset,Size,Pos}),
	    io:format("  expand truncated binary ok",[]),
	    ['#CDVBin',SOffset,SSize,SPos] = proplists:get_value(sub_bin,Dict),
            {ok,<<_:SSize/binary>>} =
		crashdump_viewer:expand_binary({SOffset,SSize,SPos}),
	    io:format("  expand complete sub binary ok",[]),

            ok;
        ".trunc_bytes" ->
            {ok,_,[TW]} = crashdump_viewer:general_info(),
            {match,_} = re:run(TW,"CRASH DUMP SIZE LIMIT REACHED"),
	    io:format("  size limit information ok",[]),
            ok;
        ".unicode" ->
            #proc{pid=Pid0} =
                lists:keyfind("'unicode_reg_name_αβ'",#proc.name,Procs),
            Pid = pid_to_list(Pid0),
	    {ok,#proc{},[]} = crashdump_viewer:proc_details(Pid),
            io:format("  unicode registered name ok",[]),

	    {ok,[#ets_table{id="'tab_αβ'",name="'tab_αβ'"}],[]} =
                crashdump_viewer:ets_tables(Pid),
            io:format("  unicode table name ok",[]),

            ok;
        ".maps" ->
	    %% I registered a process as aaaaaaaa_maps in the map dump
	    %% to make sure it will be the first in the list when sorted
	    %% on names.
	    [#proc{pid=Pid0,name=Name}|_Rest] = lists:keysort(#proc.name,Procs),
            "aaaaaaaa_maps" = Name,
	    Pid = pid_to_list(Pid0),
	    {ok,ProcDetails=#proc{},[]} = crashdump_viewer:proc_details(Pid),
	    io:format("  process details ok",[]),

	    #proc{dict=Dict} = ProcDetails,
            %% io:format("~p\n", [Dict]),
            Maps = crashdump_helper:create_maps(),
            Maps = proplists:get_value(maps,Dict),
            io:format("  maps ok",[]),
            ok;
        ".persistent_terms" ->
	    %% I registered a process as aaaaaaaa_persistent_term in
	    %% the dump to make sure it will be the first in the list
	    %% when sorted on names.
	    [#proc{pid=Pid0,name=Name}|_Rest] = lists:keysort(#proc.name,Procs),
            "aaaaaaaa_persistent_terms" = Name,
	    Pid = pid_to_list(Pid0),
	    {ok,ProcDetails=#proc{},[]} = crashdump_viewer:proc_details(Pid),
	    io:format("  process details ok",[]),

	    #proc{dict=Dict} = ProcDetails,
            %% io:format("~p\n", [Dict]),
            Pts = crashdump_helper:create_persistent_terms(),
            Pts = proplists:get_value(pts,Dict),
            io:format("  persistent terms ok",[]),
            ok;
	_ ->
	    ok
    end,
    ok.

verify_binaries([H|T1], [H|T2]) ->
    %% Heap binary.
    verify_binaries(T1, T2);
verify_binaries([Bin|T1], [['#CDVBin',Offset,Size,Pos]|T2]) ->
    %% Refc binary.
    {ok,<<Bin:Size/binary>>} = crashdump_viewer:expand_binary({Offset,Size,Pos}),
    verify_binaries(T1, T2);
verify_binaries([], []) ->
    ok.

lookat_all_pids([],_,_) ->
    ok;
lookat_all_pids([#proc{pid=Pid0}|Procs],TruncAllowed,IncompAllowed) ->
    Pid = pid_to_list(Pid0),
    {ok,_ProcDetails=#proc{},ProcTW} = crashdump_viewer:proc_details(Pid),
    {ok,_Ets,EtsTW} = crashdump_viewer:ets_tables(Pid),
    {ok,_Timers,TimersTW} = crashdump_viewer:timers(Pid),
    case {ProcTW,EtsTW,TimersTW} of
        {[],[],[]} ->
            ok;
        {["WARNING: This process has an incomplete heap."++_],[],[]}
          when IncompAllowed ->
            ok;  % native libs, literals might not be included in dump
        _ when TruncAllowed ->
            ok; % truncated dump
        TWs ->
            ct:fail({unexpected_warning,TWs})
    end,
    lookat_all_pids(Procs,TruncAllowed,IncompAllowed).

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
	    CD3 = dump_with_args(DataDir,Rel,"instr","+Muatags true"),
	    CD4 = dump_with_strange_module_name(DataDir,Rel,"strangemodname"),
            CD5 = dump_with_size_limit_reached(DataDir,Rel,"trunc_bytes"),
            CD6 = dump_with_unicode_atoms(DataDir,Rel,"unicode"),
            CD7 = dump_with_maps(DataDir,Rel,"maps"),
            CD8 = dump_with_persistent_terms(DataDir,Rel,"persistent_terms"),
            TruncDumpMod = truncate_dump_mod(CD1),
            TruncatedDumpsBinary = truncate_dump_binary(CD1),
	    {[CD1,CD2,CD3,CD4,CD5,CD6,CD7,CD8,
              TruncDumpMod|TruncatedDumpsBinary],
             DosDump};
	_ ->
	    {[CD1,CD2], DosDump}
    end.

truncate_dump_mod(File) ->
    {ok,Bin} = file:read_file(File),
    ModNameBin = atom_to_binary(?helper_mod,latin1),
    NewLine = case os:type() of
                  {win32,_} -> <<"\r\n">>;
                  _ -> <<"\n">>
              end,
    RE = <<NewLine/binary,"=mod:",ModNameBin/binary,
           NewLine/binary,"Current size: [0-9]*",
           NewLine/binary,"Current attributes: ...">>,
    {match,[{Pos,Len}]} = re:run(Bin,RE),
    Size = Pos + Len,
    <<Truncated:Size/binary,_/binary>> = Bin,
    DumpName = filename:rootname(File) ++ ".trunc_mod",
    file:write_file(DumpName,Truncated),
    DumpName.

truncate_dump_binary(File) ->
    {ok,Bin} = file:read_file(File),
    BinTag = <<"\n=binary:">>,
    Colon = <<":">>,
    NewLine = case os:type() of
                  {win32,_} -> <<"\r\n">>;
                  _ -> <<"\n">>
              end,
    %% Split after "our binary" created by crashdump_helper
    %% (it may not be the first binary).
    RE = <<"\n=binary:(?=[0-9A-Z]+",NewLine/binary,"FF:AQID)">>,
    [StartBin,AfterTag] = re:split(Bin,RE,[{parts,2}]),
    [AddrAndSize,BinaryAndRest] = binary:split(AfterTag,Colon),
    [Binary,_Rest] = binary:split(BinaryAndRest,NewLine),
    TruncSize = byte_size(Binary) - 2,
    <<TruncBinary:TruncSize/binary,_/binary>> = Binary,
    TruncName = filename:rootname(File) ++ ".trunc_bin",
    write_trunc_files(TruncName,StartBin,
                      [BinTag,AddrAndSize,Colon,TruncBinary],1).

write_trunc_files(TruncName0,Bin,[Part|Parts],N) ->
    TruncName = TruncName0++integer_to_list(N),
    Bin1 = <<Bin/binary,Part/binary>>,
    ok = file:write_file(TruncName,Bin1),
    [TruncName|write_trunc_files(TruncName0,Bin1,Parts,N+1)];
write_trunc_files(_,_,[],_) ->
    [].


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

dump_with_size_limit_reached(DataDir,Rel,DumpName) ->
    Tmp = dump_with_args(DataDir,Rel,DumpName,""),
    {ok,#file_info{size=Max}} = file:read_file_info(Tmp),
    ok = file:delete(Tmp),
    dump_with_size_limit_reached(DataDir,Rel,DumpName,Max).

dump_with_size_limit_reached(DataDir,Rel,DumpName,Max) ->
    Bytes = max(15,rand:uniform(Max)),
    CD = dump_with_args(DataDir,Rel,DumpName,
                        "-env ERL_CRASH_DUMP_BYTES " ++
                            integer_to_list(Bytes)),
    {ok,#file_info{size=Size}} = file:read_file_info(CD),
    if Size =< Bytes ->
            %% This means that the dump was actually smaller than the
            %% randomly selected truncation size, so we'll just do it
            %% again with a smaller number
            ok = file:delete(CD),
            dump_with_size_limit_reached(DataDir,Rel,DumpName,Size-3);
       true ->
            CD
    end.

dump_with_unicode_atoms(DataDir,Rel,DumpName) ->
    Opt = rel_opt(Rel),
    Pz = "-pz \"" ++ filename:dirname(code:which(?MODULE)) ++ "\"",
    PzOpt = [{args,Pz}],
    {ok,N1} = ?t:start_node(n1,peer,Opt ++ PzOpt),
    {ok,_Pid} = rpc:call(N1,crashdump_helper_unicode,start,[]),
    CD = dump(N1,DataDir,Rel,DumpName),
    ?t:stop_node(n1),
    CD.

dump_with_maps(DataDir,Rel,DumpName) ->
    Opt = rel_opt(Rel),
    Pz = "-pz \"" ++ filename:dirname(code:which(?MODULE)) ++ "\"",
    PzOpt = [{args,Pz}],
    {ok,N1} = ?t:start_node(n1,peer,Opt ++ PzOpt),
    {ok,_Pid} = rpc:call(N1,crashdump_helper,dump_maps,[]),
    CD = dump(N1,DataDir,Rel,DumpName),
    ?t:stop_node(n1),
    CD.

dump_with_persistent_terms(DataDir,Rel,DumpName) ->
    Opt = rel_opt(Rel),
    Pz = "-pz \"" ++ filename:dirname(code:which(?MODULE)) ++ "\"",
    PzOpt = [{args,Pz}],
    {ok,N1} = ?t:start_node(n1,peer,Opt ++ PzOpt),
    {ok,_Pid} = rpc:call(N1,crashdump_helper,dump_persistent_terms,[]),
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

rel_opt(current) ->
    [];
rel_opt(Rel) ->
    [{erl,[{release,lists:concat([Rel,"_latest"])}]}].

dump_prefix(current) ->
    dump_prefix(erlang:system_info(otp_release));
dump_prefix(Rel) ->
    lists:concat(["r",Rel,"_dump."]).

get_rel_from_dump_name(File) ->
    Name = filename:basename(File),
    ["r"++Rel|_] = string:split(Name,"_"),
    list_to_integer(Rel).

compat_rel(current) ->
    "";
compat_rel(Rel) ->
    lists:concat(["+R",Rel," "]).
