%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
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

-module(crashdump_viewer_SUITE).

%% Test functions
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 translate/1,start/1,fini/1,load_file/1,
	 non_existing/1,not_a_crashdump/1,old_crashdump/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").
-include_lib("kernel/include/file.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(default_timeout, ?t:minutes(30)).
-define(sl_alloc_vsns,[r9b]).
-define(failed_file,"failed-cases.txt").

init_per_testcase(_Case, Config) ->
    DataDir = ?config(data_dir,Config),
    Fs = filelib:wildcard(filename:join(DataDir,"*translated*")),
    lists:foreach(fun(F) -> file:delete(F) end,Fs),
    catch crashdump_viewer:stop(),
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(Case, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
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
    [translate, load_file, non_existing, not_a_crashdump,
     old_crashdump].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(doc) ->
    ["Create a lot of crashdumps which can be used in the testcases below"];
init_per_suite(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?default_timeout),
    delete_saved(Config),
    application:start(inets), % will be using the http client later
    httpc:set_options([{ipfamily,inet6fb4}]),
    DataDir = ?config(data_dir,Config),
    Rels = [R || R <- [r14b,r15b], ?t:is_release_available(R)] ++ [current],
    io:format("Creating crash dumps for the following releases: ~p", [Rels]),
    AllDumps = create_dumps(DataDir,Rels),
    ?t:timetrap_cancel(Dog),
    [{dumps,AllDumps}|Config].

delete_saved(Config) ->
    DataDir = ?config(data_dir,Config),
    file:delete(filename:join(DataDir,?failed_file)),
    SaveDir = filename:join(DataDir,"save"),
    Dumps = filelib:wildcard(filename:join(SaveDir,"*")),
    lists:foreach(fun(F) -> file:delete(F) end, Dumps),
    file:del_dir(SaveDir),
    ok.


translate(suite) ->
    [];
translate(doc) ->
    ["Test that crash dumps from OTP R9B can be translated"];
translate(Config) when is_list(Config) ->
    DataDir = ?config(data_dir,Config),
    OutFile = filename:join(DataDir,"translated"),
    
    R9BFiles = filelib:wildcard(filename:join(DataDir,"r9b_dump.*")),
    AllFiles = R9BFiles,
    lists:foreach(
      fun(File) ->
	      io:format("Translating file: ~s~n",[File]),
	      ok = crashdump_translate:old2new(File,OutFile),
	      check_result(File,OutFile)
      end,
      AllFiles),
    ok.

start(suite) ->
    [];
start(doc) ->
    ["Test start and stop of the Crashdump Viewer"];
start(Config) when is_list(Config) ->
    %% Set a much shorter timeout here... We don't have all the time in world.
    AngryDog = ?t:timetrap(?t:seconds(30)),
    Port = start_cdv(),
    true = is_pid(whereis(crashdump_viewer_server)),
    true = is_pid(whereis(web_tool)),
    Html = contents(Port,"start_page"),
    "Welcome to the Web BasedErlang Crash Dump Analyser" = strip(Html),
    ok = crashdump_viewer:stop(),
    timer:sleep(10), % give some time to stop
    undefined = whereis(crashdump_viewer_server),
    undefined = whereis(web_tool),
    Url = cdv_url(Port,"start_page"),
    {error,_} = httpc:request(Url),
%    exit(whereis(httpc_manager),kill),
    ?t:timetrap_cancel(AngryDog),
    ok.

fini(Config) when is_list(Config) ->
    ok.

load_file(suite) ->
    [];
load_file(doc) ->
    ["Load files into the tool and view all pages"];
load_file(Config) when is_list(Config) ->
    case ?t:is_debug() of
	true ->
	    {skip,"Debug-compiled emulator -- far too slow"};
	false ->
	    load_file_1(Config)
    end.


load_file_1(Config) ->
    DataDir = ?config(data_dir,Config),
    Port = start_cdv(),

    AllFiles = filelib:wildcard(filename:join(DataDir,"r*_dump.*")),
    lists:foreach(
      fun(File) ->
	      browse_file(Port,File),
	      special(Port,File)
      end,
      AllFiles),
    ok = crashdump_viewer:stop().

non_existing(suite) ->
    [];
non_existing(doc) ->
    ["Try to load nonexisting file"];
non_existing(Config) when is_list(Config) ->
    Port = start_cdv(),
    Url = "http://localhost:"++Port++"/cdv_erl/crashdump_viewer/read_file",
    Html = request_sync(post,{Url,[],[],"path=nonexistingfile"}),
    "Please wait..." = title(Html),
    "An error occured:nonexistingfile is not an Erlang crash dump" = 
	strip(wait(10,Port,"redirect")),
    ok = crashdump_viewer:stop().

old_crashdump(doc) ->
    ["Try to load nonexisting file"];
old_crashdump(Config) when is_list(Config) ->
    Port = start_cdv(),
    DataDir = ?config(data_dir, Config),
    OldCrashDump = filename:join(DataDir, "old_format.dump"),
    Url = "http://localhost:"++Port++"/cdv_erl/crashdump_viewer/read_file",
    Html = request_sync(post,{Url,[],[],"path="++OldCrashDump}),
    "Please wait..." = title(Html),
    Str = "An error occured:The crashdump "++OldCrashDump++
	" is in the pre-R10B format, which is no longer supported.",
    Str = strip(wait(10,Port,"redirect")),
    ok = crashdump_viewer:stop().


not_a_crashdump(suite) ->
    [];
not_a_crashdump(doc) ->
    ["Try to load a file which is not an erlang crashdump"];
not_a_crashdump(Config) when is_list(Config) ->
    Port = start_cdv(),
    NoCrashdump = code:which(?MODULE),
    Url = "http://localhost:"++Port++"/cdv_erl/crashdump_viewer/read_file",
    Html = request_sync(post,{Url,[],[],"path="++NoCrashdump}),
    "Please wait..." = title(Html),
    Str = "An error occured:"++NoCrashdump++" is not an Erlang crash dump",
    Str = strip(wait(10,Port,"redirect")),
    ok = crashdump_viewer:stop(),
%    exit(whereis(httpc_manager),kill),
    ok.
    


end_per_suite(doc) ->
    ["Remove generated crashdumps"];
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
start_cdv() ->
    ?t:capture_start(),
    ok = crashdump_viewer:start(),
    "WebTool is available at http://localhost:" ++ Where = 
	lists:flatten(?t:capture_get()),
    ?t:capture_stop(),
    [Port|_] = string:tokens(Where,"/"),
    Port.


check_result(File,OutFile) ->
    {ok,#file_info{size=FS}} = file:read_file_info(File),
    {ok,#file_info{size=OFS}} = file:read_file_info(OutFile),
    Rel = 
	if OFS > 0 -> FS/OFS;
	   true -> 1.25
	end,
    if Rel>0.75, Rel<1.25 -> ok;
       true -> ?t:fail({unreasonable_size,File,FS,OFS})
    end,
    {ok,Fd} = file:open(OutFile,[read]),
    "=erl_crash_dump:0.0\n" = io:get_line(Fd,''),
    case is_truncated(File) of
	true ->
	    ok;
	false ->
	    {ok,_} = file:position(Fd,{eof,-5}),
	    case io:get_line(Fd,'') of
		"=end\n"  -> ok;
		Other -> ?t:fail({truncated,File,Other})
	    end
    end,
    ok = file:close(Fd).


%% Read a page and check that the page title matches Title
contents(Port,Link) ->
    Url = cdv_url(Port,Link),
    request_sync(get,{Url,[]}).

cdv_url(Port,Link) ->
    "http://localhost:" ++ Port ++ "/cdv_erl/crashdump_viewer/" ++ Link.

request_sync(Method,HTTPReqCont) ->
    case httpc:request(Method,
		      HTTPReqCont,
		      [{timeout,30000}],
		      [{full_result, false}]) of
	{ok,{200,Html}} ->
	    Html;
	{ok,{Code,Html}} ->
	    io:format("~s\n", [Html]),
	    io:format("Received ~w from httpc:request(...) with\nMethod=~w\n"
		      "HTTPReqCont=~p\n",
		      [Code,Method,HTTPReqCont]),
	    ?t:fail();
	Other ->
	    io:format(
	      "Received ~w from httpc:request(...) with\nMethod=~w\n"
	      "HTTPReqCont=~p\n",
	      [Other,Method,HTTPReqCont]),
	    ?t:fail()
    end.




strip([$<|Html]) ->
    strip(drop_tag(Html));
strip([$\n|Html]) -> 
    strip(Html);
strip([X|Html]) ->
    [X|strip(Html)];
strip([]) ->
    [].
drop_tag([$>|Html]) ->
    Html;
drop_tag([_|Html]) ->
    drop_tag(Html).

title(Port,Link,Title) ->
    Html = contents(Port,Link),
    Title = title(Html).

wait(0,_Port,Link) ->
    ?t:fail({wait,Link,timeout});
wait(Time,Port,Link) ->
    Html = contents(Port,Link),
    case title(Html) of
	"Please wait..." ->
	    timer:sleep(1000),
	    wait(Time-1,Port,Link);
	_Title ->
	    Html
    end.

title([$<,$T,$I,$T,$L,$E,$>|Html]) ->
    title_end(Html);
title([_|Html]) ->
    title(Html);
title([]) ->
    [].

title_end([$<,$/,$T,$I,$T,$L,$E,$>|_]) ->
    [];
title_end([X|Html]) ->
    [X|title_end(Html)].


%%%-----------------------------------------------------------------
%%% General check of what is displayed for a dump
browse_file(Port,File) ->
    io:format("Browsing file: ~s~n",[File]),

    %% The page where a filename can be entered
    title(Port,"read_file_frame","Read File"),

    %% Load a file
    Url = "http://localhost:"++Port++"/cdv_erl/crashdump_viewer/read_file",
    Html = request_sync(post,{Url,[],[],"path="++File}),
    "Please wait..." = title(Html),
    "Crashdump Viewer Start Page" = title(wait(10,Port,"start_page")),
    
    %% The frame with the initial information for a dump
    title(Port,"initial_info_frame","General Information"),
    
    %% Topmost frame of the page
    FilenameFrame = contents(Port,"filename_frame"),
    Match = "FilenameCrashdump currently viewed:" ++ File,
    true = lists:prefix(Match,strip(FilenameFrame)),
    
    %% Toggle a menu item and check that it explodes/collapses
    title(Port,"menu_frame","Menu"),
    exploded = toggle_menu(Port),
    collapsed = toggle_menu(Port),
    
    %% Open each page in menu and check that correct title is shown
    title(Port,"general_info","General Information"),
    title(Port,"processes","Process Information"),
    title(Port,"sort_procs?sort=state","Process Information"),
    title(Port,"sort_procs?sort=state","Process Information"),
    title(Port,"sort_procs?sort=pid","Process Information"),
    title(Port,"sort_procs?sort=pid","Process Information"),
    title(Port,"sort_procs?sort=msg_q_len","Process Information"),
    title(Port,"sort_procs?sort=msg_q_len","Process Information"),
    title(Port,"sort_procs?sort=reds","Process Information"),
    title(Port,"sort_procs?sort=reds","Process Information"),
    title(Port,"sort_procs?sort=mem","Process Information"),
    title(Port,"sort_procs?sort=mem","Process Information"),
    title(Port,"sort_procs?sort=name","Process Information"),
    title(Port,"sort_procs?sort=name","Process Information"),
    title(Port,"sort_procs?sort=init_func","Process Information"),
    title(Port,"sort_procs?sort=init_func","Process Information"),
    title(Port,"ports","Port Information"),
    title(Port,"ets_tables","ETS Table Information"),
    title(Port,"timers","Timer Information"),
    title(Port,"fun_table","Fun Information"),
    title(Port,"atoms","Atoms"),
    title(Port,"dist_info","Distribution Information"),
    title(Port,"loaded_modules","Loaded Modules Information"),
    title(Port,"hash_tables","Hash Table Information"),
    title(Port,"index_tables","Index Table Information"),
    title(Port,"memory","Memory Information"),
    title(Port,"allocated_areas","Information about allocated areas"),
    title(Port,"allocator_info","Allocator Information"),
    
    case is_truncated(File) of
	true ->
	    ok;
	_ ->
	    proc_details(Port),
	    port_details(Port),
	    title(Port,"loaded_mod_details?mod=kernel","kernel")
    end,
    
    ok.


special(Port,File) ->
    case filename:extension(File) of
	".full_dist" ->
	    contents(Port,"processes"),
	    AllProcs = contents(Port,"sort_procs?sort=name"),

	    %% I registered a process as aaaaaaaa in the full_dist dumps 
	    %% to make sure it will be the first in the list when sorted
	    %% on names. There are some special data here, so I'll thoroughly
	    %% read the process details for this process. Other processes
	    %% are just briefly traversed.
	    {Pid,Rest1} = get_first_process(AllProcs),
	    
	    ProcDetails = contents(Port,"proc_details?pid=" ++ Pid),
	    ProcTitle = "Process " ++ Pid,
	    ProcTitle = title(ProcDetails),
	    title(Port,"ets_tables?pid="++Pid,"ETS Tables for Process "++Pid),
	    title(Port,"timers?pid="++Pid,"Timers for Process "++Pid),

	    case filename:basename(File) of
		"r10b_dump.full_dist" ->
		    [MsgQueueLink,DictLink,StackDumpLink] = 
			expand_memory_links(ProcDetails),
		    MsgQueue = contents(Port,MsgQueueLink),
		    "MsgQueue" = title(MsgQueue),
		    title(Port,DictLink,"Dictionary"),
		    title(Port,StackDumpLink,"StackDump"),
		    
		    ExpandBinaryLink = expand_binary_link(MsgQueue),
		    title(Port,ExpandBinaryLink,"Expanded binary"),
		    lookat_all_pids(Port,Rest1);
		_ ->
		    ok
	    end;
	".strangemodname" ->
	    AllMods = contents(Port,"loaded_modules"),
	    open_all_modules(Port,AllMods),
	    ok;
	%%! No longer needed - all atoms are shown on one page!!
	%% ".250atoms" ->
	%%     Html1 = contents(Port,"atoms"),
	%%     NextLink1 = next_link(Html1),
	%%     "Atoms" = title(Html1),
	%%     Html2 = contents(Port,NextLink1),
	%%     NextLink2 = next_link(Html2),
	%%     "Atoms" = title(Html2),
	%%     Html3 = contents(Port,NextLink2),
	%%     "" = next_link(Html3),
	%%     "Atoms" = title(Html3);
	_ ->
	    ok
    end,
    case filename:basename(File) of
	"r10b_dump." ++ _ ->
	    lookat_all_pids(Port,contents(Port,"processes"));
	"r11b_dump." ++ _ ->
	    lookat_all_pids(Port,contents(Port,"processes"));
	_ ->
	    ok
    end,
    ok.


lookat_all_pids(Port,Pids) ->
    case get_first_process(Pids) of
	{Pid,Rest} ->
	    ProcDetails = contents(Port,"proc_details?pid=" ++ Pid),
	    ProcTitle = "Process " ++ Pid,
	    ProcTitle = title(ProcDetails),
	    title(Port,"ets_tables?pid="++Pid,"ETS Tables for Process "++Pid),
	    title(Port,"timers?pid="++Pid,"Timers for Process "++Pid),
	    
	    MemoryLinks = expand_memory_links(ProcDetails),
	    lists:foreach(
	      fun(Link) ->
		      Cont = contents(Port,Link),
		      true = lists:member(title(Cont),
					  ["MsgQueue",
					   "Dictionary",
					   "StackDump"])
	      end,
	      MemoryLinks),
	    lookat_all_pids(Port,Rest);
	false ->
	    ok
    end.


get_first_process([]) ->
    false;
get_first_process(Html) ->
    case Html of
	"<TD><A HREF=\"./proc_details?pid=" ++ Rest ->
	    {string:sub_word(Rest,1,$"),Rest};
	[_H|T] ->
	    get_first_process(T)
    end.
	
expand_memory_links(Html) ->
    case Html of
	"<B>MsgQueue</B></TD><TD COLSPAN=3><A HREF=\"./" ++ Rest ->
	    [string:sub_word(Rest,1,$")|expand_memory_links(Rest)];
	"<B>Dictionary</B></TD><TD COLSPAN=3><A HREF=\"./" ++ Rest ->
	    [string:sub_word(Rest,1,$")|expand_memory_links(Rest)];
	"<B>StackDump</B></TD><TD COLSPAN=3><A HREF=\"./" ++ Rest ->
	    [string:sub_word(Rest,1,$")];
	[_H|T] ->
	    expand_memory_links(T);
	[] ->
	    []
    end.

expand_binary_link(Html) ->
    case Html of
	"<A HREF=\"./expand_binary?pos=" ++ Rest ->
	    "expand_binary?pos=" ++ string:sub_word(Rest,1,$");
	[_H|T] ->
	    expand_binary_link(T)
    end.

open_all_modules(Port,Modules) ->
    case get_first_module(Modules) of
	{Module,Rest} ->
	    ModuleDetails = contents(Port,"loaded_mod_details?mod=" ++ Module),
            ModTitle = http_uri:decode(Module),
	    ModTitle = title(ModuleDetails),
	    open_all_modules(Port,Rest);
	false ->
	    ok
    end.

get_first_module([]) ->
    false;
get_first_module(Html) ->
    case Html of
	"<TD><A HREF=\"loaded_mod_details?mod=" ++ Rest ->
	    {string:sub_word(Rest,1,$"),Rest};
	 [_H|T] ->
	    get_first_module(T)
    end.

%% next_link(Html) ->
%%     case Html of
%% 	"<A HREF=\"./next?pos=" ++ Rest ->
%% 	    "next?pos=" ++ string:sub_word(Rest,1,$");
%% 	[_H|T] ->
%% 	    next_link(T);
%% 	[] ->
%% 	    []
%%     end.



toggle_menu(Port) ->
    Html = contents(Port,"toggle?index=4"),
    check_toggle(Html).

check_toggle(Html) ->
    case Html of
	"<A HREF=\"./toggle?index=4\"><IMG SRC=\"/crashdump_viewer/collapsd.gif\"" ++ _ ->
	    collapsed;
	"<A HREF=\"./toggle?index=4\"><IMG SRC=\"/crashdump_viewer/exploded.gif\"" ++ _ ->
	    exploded;
	[_H|T] ->
	    check_toggle(T)
    end.
	    

proc_details(Port) ->
    ProcDetails = contents(Port,"proc_details?pid=<0.0.0>"),
    "Process <0.0.0>" = title(ProcDetails),

    ExpandLink = expand_link(ProcDetails),
    title(Port,ExpandLink,"StackDump"),

    Unknown = contents(Port,"proc_details?pid=<0.9999.0>"),
    "Could not find process: <0.9999.0>" = title(Unknown).

expand_link(Html) ->
    case Html of
	"<B>StackDump</B></TD><TD COLSPAN=3><A HREF=\"./" ++ Rest ->
	    string:sub_word(Rest,1,$");
	[_H|T] ->
	    expand_link(T)
    end.


port_details(Port) ->
    Port0 = contents(Port,"port?port=Port<0.0>"),
    Port1 = contents(Port,"port?port=Port<0.1>"),
    case title(Port0) of
	"#Port<0.0>" -> % R16 or later
	    "Could not find port: #Port<0.1>" = title(Port1);
	"Could not find port: #Port<0.0>" -> % R15 or earlier
	    "#Port<0.1>" = title(Port1)
    end.

is_truncated(File) ->
    case filename:extension(filename:rootname(File)) of
	".trunc" -> true;
	_ -> false
    end.


%%%-----------------------------------------------------------------
%%% 
create_dumps(DataDir,Rels) ->
    create_dumps(DataDir,Rels,[]).
create_dumps(DataDir,[Rel|Rels],Acc) ->
    Fun = fun() -> do_create_dumps(DataDir,Rel) end,
    Pa = filename:dirname(code:which(?MODULE)),
    {SlAllocDumps,Dumps,DosDump} = 
	?t:run_on_shielded_node(Fun, compat_rel(Rel) ++ "-pa \"" ++ Pa ++ "\""),
    create_dumps(DataDir,Rels,SlAllocDumps ++ Dumps ++ Acc ++ DosDump);
create_dumps(_DataDir,[],Acc) ->
    Acc.

do_create_dumps(DataDir,Rel) ->
    SlAllocDumps = 
	case lists:member(Rel,?sl_alloc_vsns) of
	    true ->
		[dump_with_args(DataDir,Rel,"no_sl_alloc","+Se false"),
		 dump_with_args(DataDir,Rel,"sl_alloc_1","+Se true +Sr 1"),
		 dump_with_args(DataDir,Rel,"sl_alloc_2","+Se true +Sr 2")];
	    false ->
		[]
	end,
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
	    {SlAllocDumps, [CD1,CD2,CD3,CD4], DosDump};
	_ ->
	    {SlAllocDumps, [CD1,CD2], DosDump}
    end.


%% Create a dump which has two visible nodes, one hidden and one
%% not connected node, and with monitors and links between nodes.
full_dist_dump(DataDir,Rel) ->
    Opt = rel_opt(Rel),
    Pz = "-pz \"" ++ filename:dirname(code:which(?MODULE)) ++ "\"",
    PzOpt = [{args,Pz}],
    {ok,N1} = ?t:start_node(n1,peer,Opt ++ PzOpt),
    {ok,N2} = ?t:start_node(n2,peer,Opt ++ PzOpt),
    {ok,N3} = ?t:start_node(n3,peer,Opt ++ PzOpt),
    {ok,N4} = ?t:start_node(n4,peer,Opt ++ [{args,"-hidden " ++ Pz}]),
    Creator = self(),

    HelperMod = crashdump_helper,
    
    P1 = rpc:call(N1,HelperMod,n1_proc,[N2,Creator]),
    P2 = rpc:call(N2,HelperMod,remote_proc,[P1,Creator]),
    P3 = rpc:call(N3,HelperMod,remote_proc,[P1,Creator]),
    P4 = rpc:call(N4,HelperMod,remote_proc,[P1,Creator]),
    
    get_response(P2),
    get_response(P3),
    get_response(P4),
    get_response(P1),

    L = lists:seq(0,255),
    BigMsg = {message,list_to_binary(L),L},    
    Port = hd(erlang:ports()),
    {aaaaaaaa,N1} ! {short,message,1,2.5,"hello world",Port,{}},
    {aaaaaaaa,N1} ! BigMsg,
    
    ?t:stop_node(N3),
    DumpName = "full_dist",
    CD = dump(N1,DataDir,Rel,DumpName),

    ?t:stop_node(N2),
    ?t:stop_node(N4),
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
    case Rel of
	_ when Rel<r15b, Rel=/=current ->
	    rpc:call(Node,os,putenv,["ERL_CRASH_DUMP_SECONDS","600"]);
	_ ->
	    ok
    end,
    rpc:call(Node,erlang,halt,[DumpName]),
    Crashdump0 = filename:join(filename:dirname(code:which(?t)),
			       "erl_crash_dump.n1"),
    Crashdump1 = filename:join(DataDir, dump_prefix(Rel)++DumpName),
    ok = rename(Crashdump0,Crashdump1),
    Crashdump1.

rename(From,To) ->
    ok = check_complete(From),
    case file:rename(From,To) of
	{error,exdev} ->
	    {ok,_} = file:copy(From,To),
	    ok = file:delete(From);
	ok ->
	    ok
    end.

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
	r9b -> [{erl,[{release,"r9b_patched"}]}];
	r9c -> [{erl,[{release,"r9c_patched"}]}];
	r10b -> [{erl,[{release,"r10b_patched"}]}];
	r11b -> [{erl,[{release,"r11b_patched"}]}];
	r12b -> [{erl,[{release,"r12b_patched"}]}];
	r13b -> [{erl,[{release,"r13b_patched"}]}];
	r14b -> [{erl,[{release,"r14b_latest"}]}]; %naming convention changed
	r15b -> [{erl,[{release,"r15b_latest"}]}];
	current -> []
    end.

dump_prefix(Rel) ->
    case Rel of
	r9b -> "r9b_dump.";
	r9c -> "r9c_dump.";
	r10b -> "r10b_dump.";
	r11b -> "r11b_dump.";
	r12b -> "r12b_dump.";
	r13b -> "r13b_dump.";
	r14b -> "r14b_dump.";
	r15b -> "r15b_dump.";
	current -> "r16b_dump."
    end.

compat_rel(Rel) ->
    case Rel of
	r9b -> "+R9 ";
	r9c -> "+R9 ";
	r10b -> "+R10 ";
	r11b -> "+R11 ";
	r12b -> "+R12 ";
	r13b -> "+R13 ";
	r14b -> "+R14 ";
	r15b -> "+R15 ";
	current -> ""
    end.
