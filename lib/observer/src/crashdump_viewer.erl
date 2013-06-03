%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2013. All Rights Reserved.
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
-module(crashdump_viewer).

%% 
%% This module is the main module in the crashdump viewer. It implements
%% the server started by webtool and the API for the crashdump viewer tool.
%% 
%% All functions in the API except configData/0 and start_link/0 are 
%% called from HTML pages via erl_scheme (mod_esi).
%% 
%% Tables
%% ------
%% cdv_menu_table: This table holds the menu which is presented in the left
%% frame of the crashdump viewer page. Each element in the table represents
%% one meny item, and the state of the item indicates if it is presently 
%% visible or not.
%% 
%% cdv_dump_index_table: This table holds all tags read from the crashdump.
%% Each tag indicates where the information about a specific item starts.
%% The table entry for a tag includes the start position for this
%% item-information. All tags start with a "=" at the beginning of
%% a line.
%%
%% Process state
%% -------------
%% file: The name of the crashdump currently viewed.
%% dump_vsn: The version number of the crashdump
%% procs_summary: Process summary represented by a list of 
%% #proc records. This is used for efficiency reasons when sorting the
%% process summary table instead of reading all processes from the
%% dump again. Note that if the dump contains more than
%% ?max_sort_process_num processes, the sort functionality is not
%% available, and the procs_summary field in the state will have the
%% value 'too_many'.
%% sorted: string(), indicated what item was last sorted in process summary.
%% This is needed so reverse sorting can be done.
%% shared_heap: 'true' if crashdump comes from a system running shared heap,
%% else 'false'.
%% wordsize: 4 | 8, the number of bytes in a word.
%% binaries: a gb_tree containing binaries or links to binaries in the dump
%%

%% User API
-export([start/0,stop/0,script_start/0,script_start/1]).

%% Webtool API
-export([configData/0,
	 start_link/0]).
-export([start_page/2,
	 read_file_frame/2,
	 read_file/2,
	 redirect/2,
	 filename_frame/2,
	 menu_frame/2,
	 initial_info_frame/2,
	 toggle/2,
	 general_info/2,
	 processes/3,
	 proc_details/2,
	 port/2,
	 ports/3,
	 ets_tables/3,
	 internal_ets_tables/2,
	 timers/3,
	 fun_table/3,
	 atoms/3,
	 dist_info/2,
	 loaded_modules/3,
	 loaded_mod_details/2,
	 memory/2,
	 allocated_areas/2,
	 allocator_info/2,
	 hash_tables/2,
	 index_tables/2,
	 sort_procs/3,
	 expand/2,
	 expand_binary/2,
	 expand_memory/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

%% Debug support
-export([debug/1,stop_debug/0]).

-include("crashdump_viewer.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(START_PAGE,"/cdv_erl/crashdump_viewer/start_page").
-define(READ_FILE_PAGE,"/cdv_erl/crashdump_viewer/read_file?path=").
-define(SERVER, crashdump_viewer_server).
-define(call_timeout,3600000).
-define(chunk_size,1000). % number of bytes read from crashdump at a time
-define(max_line_size,100). % max number of bytes (i.e. characters) the
			    % line_head/1 function can return
-define(max_display_size,500). % max number of bytes that will be directly
				% displayed. If e.g. msg_q is longer than
				% this, it must be explicitly expanded.
-define(max_display_binary_size,50). % max size of a binary that will be
				      % directly displayed.
-define(max_sort_process_num,10000). % Max number of processes that allows
				    % sorting. If more than this number of 
				    % processes exist, they will be displayed
				    % in the order they are found in the log.
-define(items_chunk_size,?max_sort_process_num). % Number of items per chunk 
						 % when page of many items
						 % is displayed, e.g. processes,
						 % timers, funs...
						 % Must be equal to 
						 % ?max_sort_process_num!
-define(not_available,"N/A").


%% All possible tags - use macros in order to avoid misspelling in the code
-define(allocated_areas,allocated_areas).
-define(allocator,allocator).
-define(atoms,atoms).
-define(binary,binary).
-define(debug_proc_dictionary,debug_proc_dictionary).
-define(ende,ende).
-define(erl_crash_dump,erl_crash_dump).
-define(ets,ets).
-define(fu,fu).
-define(hash_table,hash_table).
-define(hidden_node,hidden_node).
-define(index_table,index_table).
-define(instr_data,instr_data).
-define(internal_ets,internal_ets).
-define(loaded_modules,loaded_modules).
-define(memory,memory).
-define(mod,mod).
-define(no_distribution,no_distribution).
-define(node,node).
-define(not_connected,not_connected).
-define(num_atoms,num_atoms).
-define(old_instr_data,old_instr_data).
-define(port,port).
-define(proc,proc).
-define(proc_dictionary,proc_dictionary).
-define(proc_heap,proc_heap).
-define(proc_messages,proc_messages).
-define(proc_stack,proc_stack).
-define(timer,timer).
-define(visible_node,visible_node).


-record(state,{file,dump_vsn,procs_summary,sorted,shared_heap=false,
	       wordsize=4,num_atoms="unknown",binaries,bg_status}).

%%%-----------------------------------------------------------------
%%% Debugging
%% Start tracing with
%% debug(Functions).
%% Functions = local | global | FunctionList
%% FunctionList = [Function]
%% Function = {FunctionName,Arity} | FunctionName
debug(F) -> 
    ttb:tracer(all,[{file,"cdv"}]), % tracing all nodes
    ttb:p(all,[call,timestamp]),
    MS = [{'_',[],[{return_trace},{message,{caller}}]}],
    tp(F,MS),
    ttb:ctp(?MODULE,stop_debug), % don't want tracing of the stop_debug func
    ok.
tp([{M,F,A}|T],MS) -> % mod:func/arity
    ttb:tpl(M,F,A,MS),
    tp(T,MS);
tp([{M,F}|T],MS) -> % mod:func
    ttb:tpl(M,F,MS),
    tp(T,MS);
tp([M|T],MS) -> % mod
    ttb:tp(M,MS), % only exported
    tp(T,MS);
tp([],_MS) ->
    ok.
stop_debug() ->
    ttb:stop([format]).

%%%-----------------------------------------------------------------
%%% User API
start() ->
    webtool:start(),
    receive after 1000 -> ok end,
    webtool:start_tools([],"app=crashdump_viewer"),
    receive after 1000 -> ok end,
    ok.

stop() ->
    webtool:stop_tools([],"app=crashdump_viewer"),
    webtool:stop().

%%%-----------------------------------------------------------------
%%% Start crashdump_viewer via the cdv script located in
%%% $OBSERVER_PRIV_DIR/bin
script_start() ->
    usage().
script_start([File]) ->
    DefaultBrowser =
	case os:type() of
	    {win32,_} -> iexplore;
	    {unix,darwin} -> open;
	    _ -> firefox
	end,
    script_start([File,DefaultBrowser]);
script_start([FileAtom,Browser]) ->
    File = atom_to_list(FileAtom),
    case filelib:is_regular(File) of
	true ->
	    io:format("Starting crashdump_viewer...\n"),
	    start(),
	    io:format("Reading crashdump..."),
	    read_file(File),
	    redirect([],[]),
	    io:format("done\n"),
	    start_browser(Browser);
	false ->
	    io:format("cdv error: the given file does not exist\n"),
	    usage()
    end.

start_browser(Browser) ->
    PortStr = integer_to_list(gen_server:call(web_tool,get_port)),
    Url = "http://localhost:" ++ PortStr ++ ?START_PAGE,
    {OSType,_} = os:type(),
    case Browser of
	none ->
	    ok;
	iexplore when OSType == win32->
	    io:format("Starting internet explorer...\n"),
	    {ok,R} = win32reg:open(""),
	    Key="\\local_machine\\SOFTWARE\\Microsoft\\IE Setup\\Setup",
	    win32reg:change_key(R,Key),
	    {ok,Val} = win32reg:value(R,"Path"),
	    IExplore=filename:join(win32reg:expand(Val),"iexplore.exe"),
	    os:cmd("\"" ++ IExplore ++ "\" " ++ Url);
	_ when OSType == win32 ->
	    io:format("Starting ~w...\n",[Browser]),
	    os:cmd("\"" ++ atom_to_list(Browser) ++ "\" " ++ Url);
	B when B==firefox; B==mozilla ->
	    io:format("Sending URL to ~w...",[Browser]),
	    BStr = atom_to_list(Browser),
	    SendCmd = BStr ++ " -raise -remote \'openUrl(" ++ Url ++ ")\'",
	    Port = open_port({spawn,SendCmd},[exit_status]),
	    receive
		{Port,{exit_status,0}} ->
		    io:format("done\n");
		{Port,{exit_status,_Error}} ->
		    io:format(" not running, starting ~w...\n",[Browser]),
		    os:cmd(BStr ++ " " ++ Url)
	    after 5000 ->
		    io:format(" failed, starting ~w...\n",[Browser]),
		    erlang:port_close(Port),
		    os:cmd(BStr ++ " " ++ Url)
	    end;
	_ ->
	    io:format("Starting ~w...\n",[Browser]),
	    os:cmd(atom_to_list(Browser) ++ " " ++ Url)
    end,
    ok.

usage() ->
    io:format(
      "\nusage: cdv file [ browser ]\n"
      "\tThe \'file\' must be an existing erlang crash dump.\n"
      "\tDefault browser is \'iexplore\' (Internet Explorer) on Windows,\n"
      "\t\'open\' on Mac OS X, or else \'firefox\'.\n",
      []).




%%%-----------------------------------------------------------------
%%% Return config data used by webtool
configData() ->
    Dir = filename:join(code:priv_dir(observer),"crashdump_viewer"),
    {crashdump_viewer,
     [{web_data,{"CrashDumpViewer",?START_PAGE}},
      {alias,{"/crashdump_viewer",Dir}},
      {alias,{"/crashdump_erts_doc",erts_docdir()}},
      {alias,{"/crashdump_doc",cdv_docdir()}},
      {alias,{erl_alias,"/cdv_erl",[?MODULE]}},
      {start,{child,{{local,?SERVER},
		     {?MODULE,start_link,[]},
		     permanent,100,worker,[?MODULE]}}}
	      ]}.

erts_docdir() ->
    ErtsVsn = erlang:system_info(version),
    RootDir = code:root_dir(),
    VsnErtsDir = filename:join(RootDir,"erts-"++ErtsVsn),
    DocDir = filename:join(["doc","html"]),
    case filelib:is_dir(VsnErtsDir) of
	true ->
	    filename:join(VsnErtsDir,DocDir);
	false ->
	    %% So this can be run in clearcase
	    filename:join([RootDir,"erts",DocDir])
    end.

cdv_docdir() ->
    ObserverDir = code:lib_dir(observer),
    filename:join([ObserverDir,"doc","html"]).

%%====================================================================
%% External functions
%%====================================================================
%%%--------------------------------------------------------------------
%%% Start the server
start_link() ->
    case whereis(?SERVER) of
	undefined ->
	    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
	Pid ->
	    {ok,Pid}
    end.

%%%-----------------------------------------------------------------
%%% If crashdump_viewer is just started, show welcome frame. Else
%%% show menu and general_info
start_page(_Env,_Input) ->
    call(start_page).

%%%-----------------------------------------------------------------
%%% Display the form for entering the file name for the crashdump
%%% to view.
read_file_frame(_Env,_Input) ->
    crashdump_viewer_html:read_file_frame().

%%%-----------------------------------------------------------------
%%% Called when the 'ok' button is clicked after entering the dump
%%% file name.
read_file(_Env,Input) ->
    call({read_file,Input}).

%%%-----------------------------------------------------------------
%%% The topmost frame of the main page. Called when a crashdump is
%%% loaded.
filename_frame(_Env,_Input) ->
    call(filename_frame).

%%%-----------------------------------------------------------------
%%% The initial information frame. Called when a crashdump is loaded.
initial_info_frame(_Env,_Input) ->
    call(initial_info_frame).

%%%-----------------------------------------------------------------
%%% The left frame of the main page. Called when a crashdump is
%%% loaded.
menu_frame(_Env,_Input) ->
    crashdump_viewer_html:menu_frame().

%%%-----------------------------------------------------------------
%%% Called when the collapsed or exploded picture in the menu is 
%%% clicked.
toggle(_Env,Input) ->
    call({toggle,Input}).

%%%-----------------------------------------------------------------
%%% The following functions are called when menu items are clicked.
general_info(_Env,_Input) ->
    call(general_info).
processes(SessionId,_Env,_Input) ->
    call({procs_summary,SessionId}).
ports(SessionId,_Env,_Input) ->
    call({ports,SessionId}).
ets_tables(SessionId,_Env,Input) ->
    call({ets_tables,SessionId,Input}).
internal_ets_tables(_Env,_Input) ->
    call(internal_ets_tables).
timers(SessionId,_Env,Input) ->
    call({timers,SessionId,Input}).
fun_table(SessionId,_Env,_Input) ->
    call({funs,SessionId}).
atoms(SessionId,_Env,_Input) ->
    call({atoms,SessionId}).
dist_info(_Env,_Input) ->
    call(dist_info).
loaded_modules(SessionId,_Env,_Input) ->
    call({loaded_mods,SessionId}).
loaded_mod_details(_Env,Input) ->
    call({loaded_mod_details,Input}).
memory(_Env,_Input) ->
    call(memory).
allocated_areas(_Env,_Input) ->
    call(allocated_areas).
allocator_info(_Env,_Input) ->
    call(allocator_info).
hash_tables(_Env,_Input) ->
    call(hash_tables).
index_tables(_Env,_Input) ->
    call(index_tables).

%%%-----------------------------------------------------------------
%%% Called when a link to a process (Pid) is clicked.
proc_details(_Env,Input) ->
    call({proc_details,Input}).

%%%-----------------------------------------------------------------
%%% Called when one of the headings in the process summary table are
%%% clicked. It sorts the processes by the clicked heading.
sort_procs(SessionId,_Env,Input) ->
    call({sort_procs,SessionId,Input}).

%%%-----------------------------------------------------------------
%%% Called when a link to a port is clicked.
port(_Env,Input) ->
    call({port,Input}).

%%%-----------------------------------------------------------------
%%% Called when the "Expand" link in a call stack (Last Calls) is
%%% clicked.
expand(_Env,Input) ->
    call({expand,Input}).

%%%-----------------------------------------------------------------
%%% Called when the "Expand" link in a stack dump, message queue or 
%%% dictionary is clicked.
expand_memory(_Env,Input) ->
    call({expand_memory,Input}).

%%%-----------------------------------------------------------------
%%% Called when "<< xxx bytes>>" link in a stack dump, message queue or 
%%% dictionary is clicked.
expand_binary(_Env,Input) ->
    call({expand_binary,Input}).

%%%-----------------------------------------------------------------
%%% Called on regular intervals while waiting for a dump to be read
redirect(_Env,_Input) ->
    call(redirect).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    ets:new(cdv_menu_table,[set,named_table,{keypos,#menu_item.index},public]),
    ets:new(cdv_dump_index_table,[ordered_set,named_table,public]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(start_page,_From,State=#state{file=undefined,bg_status=undefined})->
    Reply = crashdump_viewer_html:welcome(),
    {reply,Reply,State};
handle_call(start_page, _From, State=#state{file=undefined,bg_status={done,Page}}) ->
    {reply,Page,State};
handle_call(start_page, _From, State=#state{file=undefined,bg_status=Status}) ->
    Reply = crashdump_viewer_html:redirect(Status),
    {reply,Reply,State};
handle_call(start_page, _From, State) ->
    Reply = crashdump_viewer_html:start_page(),
    {reply,Reply,State};
handle_call({read_file,Input}, _From, _State) ->
    {ok,File} = get_value("path",httpd:parse_query(Input)),
    spawn_link(fun() -> read_file(File) end),
    Status = background_status(reading,File),
    Reply = crashdump_viewer_html:redirect(Status),
    {reply, Reply, #state{bg_status=Status}};
handle_call(redirect,_From, State=#state{bg_status={done,Page}}) ->
    {reply, Page, State#state{bg_status=undefined}};   
handle_call(redirect,_From, State=#state{bg_status=Status}) ->
    Reply = crashdump_viewer_html:redirect(Status),
    {reply, Reply, State};
handle_call(filename_frame,_From,State=#state{file=File}) ->
    Reply = crashdump_viewer_html:filename_frame(File),
    {reply,Reply,State};
handle_call(initial_info_frame,_From,State=#state{file=File}) ->
    GenInfo = general_info(File),
    [{DumpVsn,_}] = lookup_index(?erl_crash_dump),
    NumAtoms = GenInfo#general_info.num_atoms,
    {WS,SH} = parse_vsn_str(GenInfo#general_info.system_vsn,4,false),
    NumProcs = list_to_integer(GenInfo#general_info.num_procs),
    ProcsSummary = 
	if NumProcs > ?max_sort_process_num -> too_many;
	   true -> State#state.procs_summary
	end,
    NewState = State#state{dump_vsn=[list_to_integer(L) ||
					L<-string:tokens(DumpVsn,".")],
			   shared_heap=SH,
			   wordsize=WS,
			   num_atoms=NumAtoms,
			   procs_summary=ProcsSummary},
    Reply = crashdump_viewer_html:general_info(GenInfo),
    {reply,Reply,NewState};
handle_call({toggle,Input},_From,State) ->
    {ok,Index} = get_value("index",httpd:parse_query(Input)),
    do_toggle(list_to_integer(Index)),
    Reply = crashdump_viewer_html:menu_frame(),
    {reply,Reply,State};
handle_call({expand,Input},_From,State=#state{file=File}) ->
    [{"pos",Pos},{"size",Size},{"what",What},{"truncated",Truncated}] = 
	httpd:parse_query(Input),
    Expanded = get_expanded(File,list_to_integer(Pos),list_to_integer(Size)),
    TruncText = if Truncated=="true" -> "WARNING: This term is truncated!\n\n";
		   true -> ""
		end,
    Reply = 
	case {Truncated,What} of
	    {_,"LastCalls"} ->
		LastCalls = replace_all($ ,$\n,Expanded,[]),
		crashdump_viewer_html:info_page(What,[TruncText,LastCalls]);
	    {_,"StackDump"} ->
		crashdump_viewer_html:info_page(What,[TruncText,Expanded]);
	    {"false",_} ->
		crashdump_viewer_html:pretty_info_page(What,Expanded);
	    {"true",_} ->
		crashdump_viewer_html:info_page(What,[TruncText,Expanded])
	end,
    {reply,Reply,State};
handle_call({expand_memory,Input},_From,State=#state{file=File,binaries=B}) ->
    [{"pid",Pid},{"what",What}] = httpd:parse_query(Input),
    Reply = 
	case truncated_warning([{?proc,Pid}]) of
	    [] ->
		Expanded = expand_memory(File,What,Pid,B),
		crashdump_viewer_html:expanded_memory(What,Expanded);
	    _TW ->
		Info = 
		    "The crashdump is truncated in the middle of this "
		    "process' memory information, so this information "
		    "can not be extracted.",
		crashdump_viewer_html:info_page(What,Info)
	end,
    {reply,Reply,State};
handle_call({expand_binary,Input},_From,State=#state{file=File}) ->
    [{"pos",Pos0}] = httpd:parse_query(Input),
    Pos = list_to_integer(Pos0),
    Fd = open(File),
    pos_bof(Fd,Pos),
    {Bin,_Line} = get_binary(val(Fd)),
    close(Fd),
    Reply=crashdump_viewer_html:expanded_binary(io_lib:format("~p",[Bin])),
    {reply,Reply,State};
handle_call(general_info,_From,State=#state{file=File}) ->
    GenInfo=general_info(File),
    Reply = crashdump_viewer_html:general_info(GenInfo),
    {reply,Reply,State};
handle_call({procs_summary,SessionId},_From,State) ->
    TW = truncated_warning([?proc]),
    NewState = procs_summary(SessionId,TW,"pid",State#state{sorted=undefined}),
    {reply,ok,NewState};
handle_call({sort_procs,SessionId,Input}, _From, State) ->
    {ok,Sort} = get_value("sort",httpd:parse_query(Input)),
    TW = truncated_warning([?proc]),
    NewState = procs_summary(SessionId,TW,Sort,State),
    {reply,ok,NewState};
handle_call({proc_details,Input},_From,State=#state{file=File,shared_heap=SH}) ->
    {ok,Pid} = get_value("pid",httpd:parse_query(Input)),
    Reply = 
	case get_proc_details(File,Pid,State#state.dump_vsn) of
	    {ok,Proc} -> 
		TW = truncated_warning([{?proc,Pid}]),
		crashdump_viewer_html:proc_details(Pid,Proc,TW,SH);
	    {other_node,Node} -> 
		TW = truncated_warning([?visible_node,
					?hidden_node,
					?not_connected]),
		crashdump_viewer_html:nods(Node,TW);
	    not_found -> 
		crashdump_viewer_html:info_page(["Could not find process: ",
						 Pid],?space)
	end,
    {reply, Reply, State};
handle_call({port,Input},_From,State=#state{file=File}) ->
    {ok,P} = get_value("port",httpd:parse_query(Input)),
    Id = [$#|P],
    Reply = 
	case get_port(File,Id) of
	    {ok,PortInfo} ->
		TW = truncated_warning([{?port,Id}]),
		crashdump_viewer_html:port(Id,PortInfo,TW);
	    {other_node,Node} ->
		TW = truncated_warning([?visible_node,
					?hidden_node,
					?not_connected]),
		crashdump_viewer_html:nods(Node,TW);
	    not_found -> 
		crashdump_viewer_html:info_page(
		  ["Could not find port: ",Id],?space)
	end,
    {reply,Reply,State};
handle_call({ports,SessionId},_From,State=#state{file=File}) ->
    TW = truncated_warning([?port]),
    get_ports(SessionId,File,TW),
    {reply,ok,State};
handle_call({ets_tables,SessionId,Input},_From,State=#state{file=File,wordsize=WS}) ->
    {Pid,Heading} = 
	case get_value("pid",httpd:parse_query(Input)) of
	    {ok,P} -> 
		{P,["ETS Tables for Process ",P]};
	    error -> 
		{'$2',"ETS Table Information"}
	end,
    TW = truncated_warning([?ets]),
    get_ets_tables(SessionId,File,Heading,TW,Pid,WS),
    {reply,ok,State};
handle_call(internal_ets_tables,_From,State=#state{file=File,wordsize=WS}) ->
    InternalEts = get_internal_ets_tables(File,WS),
    TW = truncated_warning([?internal_ets]),
    Reply = crashdump_viewer_html:internal_ets_tables(InternalEts,TW),
    {reply,Reply,State};
handle_call({timers,SessionId,Input},_From,State=#state{file=File}) ->
    {Pid,Heading} = 
	case get_value("pid",httpd:parse_query(Input)) of
	    {ok,P} -> {P,["Timers for Process ",P]};
	    error -> {'$2',"Timer Information"}
	end,
    TW = truncated_warning([?timer]),
    get_timers(SessionId,File,Heading,TW,Pid),
    {reply,ok,State};
handle_call(dist_info,_From,State=#state{file=File}) ->
    Nods=nods(File),
    TW = truncated_warning([?visible_node,?hidden_node,?not_connected]),
    Reply = crashdump_viewer_html:nods(Nods,TW),
    {reply,Reply,State};
handle_call({loaded_mods,SessionId},_From,State=#state{file=File}) ->
    TW = truncated_warning([?mod]),
    loaded_mods(SessionId,File,TW),
    {reply,ok,State};
handle_call({loaded_mod_details,Input},_From,State=#state{file=File}) ->
    {ok,Mod} = get_value("mod",httpd:parse_query(Input)),
    ModInfo = get_loaded_mod_details(File,Mod),
    TW = truncated_warning([{?mod,Mod}]),
    Reply = crashdump_viewer_html:loaded_mod_details(ModInfo,TW),
    {reply,Reply,State};
handle_call({funs,SessionId},_From,State=#state{file=File}) ->
    TW = truncated_warning([?fu]),
    funs(SessionId,File,TW),
    {reply,ok,State};
handle_call({atoms,SessionId},_From,State=#state{file=File,num_atoms=Num}) ->
    TW = truncated_warning([?atoms,?num_atoms]),
    atoms(SessionId,File,TW,Num),
    {reply,ok,State};
handle_call(memory,_From,State=#state{file=File}) ->
    Memory=memory(File),
    TW = truncated_warning([?memory]),
    Reply = crashdump_viewer_html:memory(Memory,TW),
    {reply,Reply,State};
handle_call(allocated_areas,_From,State=#state{file=File}) ->
    AllocatedAreas=allocated_areas(File),
    TW = truncated_warning([?allocated_areas]),
    Reply = crashdump_viewer_html:allocated_areas(AllocatedAreas,TW),
    {reply,Reply,State};
handle_call(allocator_info,_From,State=#state{file=File}) ->
    SlAlloc=allocator_info(File),
    TW = truncated_warning([?allocator]),
    Reply = crashdump_viewer_html:allocator_info(SlAlloc,TW),
    {reply,Reply,State};
handle_call(hash_tables,_From,State=#state{file=File}) ->
    HashTables=hash_tables(File),
    TW = truncated_warning([?hash_table,?index_table]),
    Reply = crashdump_viewer_html:hash_tables(HashTables,TW),
    {reply,Reply,State};
handle_call(index_tables,_From,State=#state{file=File}) ->
    IndexTables=index_tables(File),
    TW = truncated_warning([?hash_table,?index_table]),
    Reply = crashdump_viewer_html:index_tables(IndexTables,TW),
    {reply,Reply,State}.



%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({background_done,{Page,File,Binaries},Dict}, State) ->
    lists:foreach(fun({Key,Val}) -> put(Key,Val) end, Dict),
    {noreply, State#state{file=File,binaries=Binaries,bg_status={done,Page}}};
handle_cast({background_status,Status}, State) ->
    {noreply, State#state{bg_status=Status}}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------    
call(Request) ->
     gen_server:call(?SERVER,Request,?call_timeout).

cast(Msg) ->
    gen_server:cast(?SERVER,Msg).

unexpected(_Fd,{eof,_LastLine},_Where) ->
    ok; % truncated file
unexpected(Fd,{part,What},Where) ->
    skip_rest_of_line(Fd),
    io:format("WARNING: Found unexpected line in ~s:~n~s ...~n",[Where,What]);
unexpected(_Fd,What,Where) ->
    io:format("WARNING: Found unexpected line in ~s:~n~s~n",[Where,What]).

truncated_warning([]) ->
    [];
truncated_warning([Tag|Tags]) ->
    case truncated_here(Tag) of
	true -> truncated_warning();
	false -> truncated_warning(Tags)
    end.
truncated_warning() ->
    ["WARNING: The crash dump is truncated here. "
     "Some information might be missing."].

truncated_here(Tag) ->
    case get(truncated) of
	true ->
	    case get(last_tag) of
		Tag -> % Tag == {TagType,Id}
		    true;
		{Tag,_Id} ->
		    true;
		_LastTag ->
		    truncated_earlier(Tag)
	    end;
	false ->
	    false
    end.
		    

%% Check if the dump was truncated with the same tag, but earlier id.
%% Eg if this is {?proc,"<0.30.0>"}, we should warn if the dump was
%% truncated in {?proc,"<0.29.0>"} or earlier
truncated_earlier({?proc,Pid}) ->
    compare_pid(Pid,get(truncated_proc));
truncated_earlier(_Tag) ->
    false.

compare_pid("<"++Id,"<"++OtherId) ->
    Id>=OtherId;
compare_pid(_,_) ->
    false.

background_status(Action,File) ->
    SizeInfo = filesizeinfo(File), 
    background_status(Action,File,SizeInfo).

background_status(processing,File,SizeInfo) ->
    "Processing " ++ File ++ SizeInfo;
background_status(reading,File,SizeInfo) ->
    "Reading file " ++ File ++ SizeInfo.

filesizeinfo(File) ->
    case file:read_file_info(File) of
	{ok,#file_info{size=Size}} -> 
	    " (" ++ integer_to_list(Size) ++ " bytes)";
	_X ->
	    ""
    end.


open(File) ->
    {ok,Fd} = file:open(File,[read,read_ahead,raw,binary]),
    Fd.
close(Fd) ->
    erase(chunk),
    file:close(Fd).

%% Set position relative to beginning of file
%% If position is within the already read Chunk, then adjust 'chunk'
%% and 'pos' in process dictionary. Else set position in file.
pos_bof(Fd,Pos) ->
    case get(pos) of
	undefined ->
	    hard_pos_bof(Fd,Pos);
	OldPos when Pos>=OldPos ->
	    case get(chunk) of
		undefined ->
		    hard_pos_bof(Fd,Pos);
		Chunk ->
		    ChunkSize = byte_size(Chunk),
		    ChunkEnd = OldPos+ChunkSize,
		    if Pos=<ChunkEnd ->
			    Diff = Pos-OldPos,
			    put(pos,Pos),
			    put(chunk,binary:part(Chunk,Diff,ChunkEnd-Pos));
		       true ->
			    hard_pos_bof(Fd,Pos)
		    end
	    end;
	_ ->
	    hard_pos_bof(Fd,Pos)
    end.

hard_pos_bof(Fd,Pos) ->
    reset_chunk(),
    file:position(Fd,{bof,Pos}).


get_chunk(Fd) ->
    case erase(chunk) of
	undefined ->
	    case read(Fd) of
		eof -> 
		    put_pos(Fd),
		    eof;
		Other ->
		    Other
	    end;
	Bin ->
	    {ok,Bin}
    end.

read(Fd) ->
    file:read(Fd,?chunk_size).

put_chunk(Fd,Bin) ->
    {ok,Pos0} = file:position(Fd,cur),
    Pos = Pos0 - byte_size(Bin),
    put(chunk,Bin),
    put(pos,Pos).

put_pos(Fd) ->
    {ok,Pos} = file:position(Fd,cur),
    put(pos,Pos).    

reset_chunk() ->
    erase(chunk),
    erase(pos).

line_head(Fd) ->
    case get_chunk(Fd) of
	{ok,Bin} -> line_head(Fd,Bin,[],0);
	eof -> {eof,[]}
    end.
line_head(Fd,Bin,Acc,?max_line_size) ->
    put_chunk(Fd,Bin),
    {part,lists:reverse(Acc)};
line_head(Fd,<<$\n:8,Bin/binary>>,Acc,_N) ->
    put_chunk(Fd,Bin),
    lists:reverse(Acc);
line_head(Fd,<<$::8,$\r:8,$\n:8,Bin/binary>>,Acc,_N) ->
    put_chunk(Fd,Bin),
    lists:reverse(Acc);
line_head(Fd,<<$::8,$\r:8>>,Acc,N) ->
    case get_chunk(Fd) of
	{ok,Bin} -> line_head(Fd,<<$:,Bin/binary>>,Acc,N);
	eof -> {eof,lists:reverse(Acc)}
    end;
line_head(Fd,<<$::8>>,Acc,N) ->
    case get_chunk(Fd) of
	{ok,Bin} -> line_head(Fd,<<$:,Bin/binary>>,Acc,N);
	eof -> {eof,lists:reverse(Acc)}
    end;
line_head(Fd,<<$::8,Space:8,Bin/binary>>,Acc,_N) when Space=:=$ ;Space=:=$\n ->
    put_chunk(Fd,Bin),
    lists:reverse(Acc);
line_head(Fd,<<$::8,Bin/binary>>,Acc,_N) ->
    put_chunk(Fd,Bin),
    lists:reverse(Acc);
line_head(Fd,<<$\r:8,Bin/binary>>,Acc,N) ->
    line_head(Fd,Bin,Acc,N+1);
line_head(Fd,<<Char:8,Bin/binary>>,Acc,N) ->
    line_head(Fd,Bin,[Char|Acc],N+1);
line_head(Fd,<<>>,Acc,N) ->
    case get_chunk(Fd) of
	{ok,Bin} -> line_head(Fd,Bin,Acc,N);
	eof -> {eof,lists:reverse(Acc)}
    end.

skip_rest_of_line(Fd) ->
    case get_chunk(Fd) of
	{ok,Bin} -> skip(Fd,Bin);
	eof -> ok
    end.
skip(Fd,<<$\n:8,Bin/binary>>) ->
    put_chunk(Fd,Bin),
    ok;
skip(Fd,<<_Char:8,Bin/binary>>) ->
    skip(Fd,Bin);
skip(Fd,<<>>) ->
    case get_chunk(Fd) of
	{ok,Bin} -> skip(Fd,Bin);
	eof -> ok
    end.


val(Fd) ->
    case get_rest_of_line(Fd) of
	{eof,[]} -> "-1";
	[] -> "-1";
	{eof,Val} -> Val;
	Val -> Val
    end.

get_rest_of_line(Fd) ->
    case get_chunk(Fd) of
	{ok,Bin} -> get_rest_of_line_1(Fd, Bin, []);
	eof -> {eof,[]}
    end.

get_rest_of_line_1(Fd, <<$\n:8,Bin/binary>>, Acc) ->
    put_chunk(Fd, Bin),
    lists:reverse(Acc);
get_rest_of_line_1(Fd, <<$\r:8,Rest/binary>>, Acc) ->
    get_rest_of_line_1(Fd, Rest, Acc);
get_rest_of_line_1(Fd, <<Char:8,Rest/binary>>, Acc) ->
    get_rest_of_line_1(Fd, Rest, [Char|Acc]);
get_rest_of_line_1(Fd, <<>>, Acc) ->
    case get_chunk(Fd) of
	{ok,Bin} -> get_rest_of_line_1(Fd, Bin, Acc);
	eof -> {eof,lists:reverse(Acc)}
    end.

count_rest_of_line(Fd) ->
    case get_chunk(Fd) of
	{ok,Bin} -> count_rest_of_line(Fd,Bin,0);
	eof -> {eof,0}
    end.
count_rest_of_line(Fd,<<$\n:8,Bin/binary>>,N) ->
    put_chunk(Fd,Bin),
    N;
count_rest_of_line(Fd,<<$\r:8,Bin/binary>>,N) ->
    count_rest_of_line(Fd,Bin,N);
count_rest_of_line(Fd,<<_Char:8,Bin/binary>>,N) ->
    count_rest_of_line(Fd,Bin,N+1);
count_rest_of_line(Fd,<<>>,N) ->
    case get_chunk(Fd) of
	{ok,Bin} -> count_rest_of_line(Fd,Bin,N);
	eof -> {eof,N}
    end.

get_n_lines_of_tag(Fd,N) ->
    case get_chunk(Fd) of
	{ok,Bin} -> 
	    {AllOrPart,Rest,Lines} = get_n_lines_of_tag(Fd,N,Bin,[]),
	    {AllOrPart,N-Rest,Lines};
	eof ->
	    empty
    end.
get_n_lines_of_tag(Fd,N,<<"\n=",_/binary>>=Bin,Acc) ->
    put_chunk(Fd,Bin),
    {all,N-1,lists:reverse(Acc)};
get_n_lines_of_tag(Fd,0,Bin,Acc) ->
    put_chunk(Fd,Bin),
    {part,0,lists:reverse(Acc)};
get_n_lines_of_tag(Fd,N,<<$\n:8,Bin/binary>>,Acc) ->
    get_n_lines_of_tag(Fd,N-1,Bin,[$\n|Acc]);
get_n_lines_of_tag(Fd,N,<<$\r:8,Bin/binary>>,Acc) ->
    get_n_lines_of_tag(Fd,N,Bin,Acc);
get_n_lines_of_tag(Fd,N,<<Char:8,Bin/binary>>,Acc) ->
    get_n_lines_of_tag(Fd,N,Bin,[Char|Acc]);
get_n_lines_of_tag(Fd,N,<<>>,Acc) ->
    case get_chunk(Fd) of
	{ok,Bin} -> 
	    get_n_lines_of_tag(Fd,N,Bin,Acc);
	eof -> 
	    case Acc of
		[$\n|_] ->
		    {all,N,lists:reverse(Acc)};
		_ ->
		    {all,N-1,lists:reverse(Acc)}
	    end
    end.

count_rest_of_tag(Fd) ->
    case get_chunk(Fd) of
	{ok,Bin} -> count_rest_of_tag(Fd,Bin,0);
	eof -> 0
    end.
count_rest_of_tag(Fd,<<"\n=",Bin/binary>>,N) ->
    put_chunk(Fd,Bin),
    N;
count_rest_of_tag(Fd,<<$\r:8,Bin/binary>>,N) ->
    count_rest_of_tag(Fd,Bin,N);
count_rest_of_tag(Fd,<<_Char:8,Bin/binary>>,N) ->
    count_rest_of_tag(Fd,Bin,N+1);
count_rest_of_tag(Fd,<<>>,N) ->
    case get_chunk(Fd) of
	{ok,Bin} -> count_rest_of_tag(Fd,Bin,N);
	eof -> N
    end.

split(Str) ->
    split($ ,Str,[]).    
split(Char,Str) ->
    split(Char,Str,[]).
split(Char,[Char|Str],Acc) -> % match Char
    {lists:reverse(Acc),Str};
split(_Char,[$\r,$\n|Str],Acc) -> % new line
    {lists:reverse(Acc),Str};
split(_Char,[$\n|Str],Acc) -> % new line
    {lists:reverse(Acc),Str};
split(Char,[H|T],Acc) ->
    split(Char,T,[H|Acc]);
split(_Char,[],Acc) ->
    {lists:reverse(Acc),[]}.

size_or_term(Fd) ->
    size_or_term(Fd,get(pos)).
size_or_term(Fd,Pos) ->
    case count_rest_of_line(Fd) of
	{eof,Size} ->
	    {size,true,Size,Pos};
	Size when Size > ?max_display_size ->
	    {size,false,Size,Pos};
	_Size ->
	    {ok,Pos} = pos_bof(Fd,Pos),
	    val(Fd)
    end.

%%%-----------------------------------------------------------------
%%% 
get_value(Key,List) ->
    case lists:keysearch(Key,1,List) of
	{value,{Key,Value}} -> {ok,Value};
	false -> error
    end.

parse_vsn_str([],WS,false) ->
    %% If the log is translated, crashdump_translate might have written
    %% shared_heap=true in dictionary.
    case erase(shared_heap) of
	true -> {WS,true};
	_ -> {WS,false}
    end;
parse_vsn_str([],WS,SH) ->
    {WS,SH};
parse_vsn_str(Str,WS,SH) ->
    case Str of
	"[64-bit]" ++ Rest ->
	    case SH of
		false ->
		    parse_vsn_str(Rest,8,false);
		_ ->
		    {8,SH}
	    end;
	"[shared heap]" ++ Rest ->
	    case WS of
		4 -> 
		    parse_vsn_str(Rest,WS,true);
		_ ->
		    {WS,true}
	    end;
	[_Char|Rest] ->
	    parse_vsn_str(Rest,WS,SH)
    end.


%%%-----------------------------------------------------------------
%%% 
initial_menu() ->
    insert_items(
      [menu_item(0, {"./general_info","General information"},0),
       menu_item(0, {"./processes","Processes"}, 0),
       menu_item(0, {"./ports","Ports"}, 0),
       menu_item(2, "ETS tables", 0),
       menu_item(0, {"./ets_tables","ETS tables"}, 1),
       menu_item(0, {"./internal_ets_tables","Internal ETS tables"}, 1),
       menu_item(0, {"./timers","Timers"}, 0),
       menu_item(0, {"./fun_table","Fun table"}, 0),
       menu_item(0, {"./atoms","Atoms"}, 0),
       menu_item(0, {"./dist_info","Distribution information"}, 0),
       menu_item(0, {"./loaded_modules","Loaded modules"}, 0),
       menu_item(2, "Internal Tables", 0),
       menu_item(0, {"./hash_tables","Hash tables"}, 1),
       menu_item(0, {"./index_tables","Index tables"}, 1),
       menu_item(3, "Memory information", 0),
       menu_item(0, {"./memory","Memory"}, 1),
       menu_item(0, {"./allocated_areas","Allocated areas"}, 1),
       menu_item(0, {"./allocator_info","Allocator information"}, 1),
       menu_item(2, "Documentation", 0),
       menu_item(0, {"/crashdump_doc/crashdump_help.html",
		     "Crashdump Viewer help"}, 1,"doc"),
       menu_item(0, {"/crashdump_erts_doc/crash_dump.html",
		     "How to interpret Erlang crashdumps"}, 1,"doc")]).
    
menu_item(Children,Text,Depth) ->
    menu_item(Children,Text,Depth,"main").
menu_item(Children,Text,Depth,Target) ->
    #menu_item{picture=get_pic(Children),
	       text=Text,
	       depth=Depth,
	       children=Children,
	       state=if Depth==0 -> true; true -> false end,
	       target=Target}.

insert_items(Items) ->
    insert_items(Items,1).
insert_items([Item|Items],Index) ->
    ets:insert(cdv_menu_table,Item#menu_item{index=Index}),
    insert_items(Items,Index+1);
insert_items([],_) ->
    ok.

get_pic(0) ->
    "";
get_pic(_) ->
     "/crashdump_viewer/collapsd.gif".

do_toggle(Index) ->
    [Item]= ets:lookup(cdv_menu_table,Index),
    case toggle_children(Index,Index+Item#menu_item.children,
			 Item#menu_item.depth+1,undefined) of
	true ->
	    ets:insert(cdv_menu_table,
		       Item#menu_item{picture=
				      "/crashdump_viewer/exploded.gif"});
	false ->
	    ets:insert(cdv_menu_table,
		       Item#menu_item{picture=
				      "/crashdump_viewer/collapsd.gif"})
    end.

toggle_children(Index,Max,_Depth,ToggleState) when Index>Max->
    ToggleState;
toggle_children(Index,Max,Depth,ToggleState) ->
    case ets:lookup(cdv_menu_table,Index+1) of
	[#menu_item{depth=Depth}=Child] ->
	    NewState =  not Child#menu_item.state,
	    ets:insert(cdv_menu_table,Child#menu_item{state=NewState}),
	    toggle_children(Index+1,Max,Depth,NewState);
	_ ->
	    toggle_children(Index+1,Max,Depth,ToggleState)
    end.

%%%-----------------------------------------------------------------
%%% Traverse crash dump and insert index in table for each heading
%%% 
%%% This function is executed in a background process in order to
%%% avoid a timeout in the web browser. The browser displays "Please
%%% wait..." while this is going on.
%%%
%%% Variable written to process dictionary in this function are copied
%%% to the crashdump_viewer_server when the function is completed (see
%%% background_done/1).
read_file(File) ->
    case file:read_file_info(File) of
	{ok,#file_info{type=regular,access=FileA}} when FileA=:=read;
							FileA=:=read_write ->
	    Fd = open(File),
	    case read(Fd) of
		{ok,<<$=:8,TagAndRest/binary>>} ->
		    {Tag,Id,Rest,N1} = tag(Fd,TagAndRest,1),
		    case Tag of
			?erl_crash_dump ->
			    reset_index_table(),
			    insert_index(Tag,Id,N1+1),
			    put(last_tag,{Tag,""}),
			    Status = background_status(processing,File),
			    background_status(Status),
			    indexify(Fd,Rest,N1),
			    check_if_truncated(),
			    initial_menu(),
			    Binaries = read_binaries(Fd),
			    R = crashdump_viewer_html:start_page(),
			    close(Fd),
			    background_done({R,File,Binaries});
			_Other ->
			    R = crashdump_viewer_html:error(
				  "~s is not an Erlang crash dump~n",
				  [File]),
			    close(Fd),
			    background_done({R,undefined,undefined})
		    end;
		{ok,<<"<Erlang crash dump>",_Rest/binary>>} -> 
		    %% old version - no longer supported
		    R = crashdump_viewer_html:error(
			  "The crashdump ~s is in the pre-R10B format, "
			  "which is no longer supported.~n",
			  [File]),
		    close(Fd),
		    background_done({R,undefined,undefined});
		_Other ->
		    R = crashdump_viewer_html:error(
			  "~s is not an Erlang crash dump~n",
			  [File]),
		    close(Fd),
		    background_done({R,undefined,undefined})
	    end;
	_other ->
	    R = crashdump_viewer_html:error("~s is not an Erlang crash dump~n",
					    [File]),
	    background_done({R,undefined,undefined})
    end.

indexify(Fd,Bin,N) ->
    case binary:match(Bin,<<"\n=">>) of
	{Start,Len} ->
	    Pos = Start+Len,
	    <<_:Pos/binary,TagAndRest/binary>> = Bin,
	    {Tag,Id,Rest,N1} = tag(Fd,TagAndRest,N+Pos),
	    insert_index(Tag,Id,N1+1), % +1 to get past newline
	    put(last_tag,{Tag,Id}),
	    indexify(Fd,Rest,N1);
	nomatch ->
	    case read(Fd) of
		{ok,Chunk0} when is_binary(Chunk0) ->
		    {Chunk,N1} =
			case binary:last(Bin) of
			    $\n ->
				{<<$\n,Chunk0/binary>>,N+byte_size(Bin)-1};
			    _ ->
				{Chunk0,N+byte_size(Bin)}
			end,
		    indexify(Fd,Chunk,N1);
		eof ->
		    eof
	    end
    end.

tag(Fd,Bin,N) ->
    tag(Fd,Bin,N,[],[],tag).
tag(_Fd,<<$\n:8,_/binary>>=Rest,N,Gat,Di,_Now) ->
    {tag_to_atom(lists:reverse(Gat)),lists:reverse(Di),Rest,N};
tag(Fd,<<$\r:8,Rest/binary>>,N,Gat,Di,Now) ->
    tag(Fd,Rest,N+1,Gat,Di,Now);
tag(Fd,<<$::8,IdAndRest/binary>>,N,Gat,Di,tag) ->
    tag(Fd,IdAndRest,N+1,Gat,Di,id);
tag(Fd,<<Char:8,Rest/binary>>,N,Gat,Di,tag) ->
    tag(Fd,Rest,N+1,[Char|Gat],Di,tag);
tag(Fd,<<Char:8,Rest/binary>>,N,Gat,Di,id) ->
    tag(Fd,Rest,N+1,Gat,[Char|Di],id);
tag(Fd,<<>>,N,Gat,Di,Now) ->
    case read(Fd) of
	{ok,Chunk} when is_binary(Chunk) ->
	    tag(Fd,Chunk,N,Gat,Di,Now);
        eof ->
	    {tag_to_atom(lists:reverse(Gat)),lists:reverse(Di),<<>>,N}
    end.

check_if_truncated() ->
    case get(last_tag) of
	{?ende,_} ->
	    put(truncated,false),
	    put(truncated_proc,false);
	TruncatedTag ->
	    put(truncated,true),
	    find_truncated_proc(TruncatedTag)
    end.
	    
find_truncated_proc({?atoms,_Id}) ->
    put(truncated_proc,false);
find_truncated_proc({Tag,Pid}) ->
    case is_proc_tag(Tag) of
	true -> 
	    put(truncated_proc,Pid);
	false -> 
	    %% This means that the dump is truncated between ?proc and
	    %% ?proc_heap => memory info is missing for all procs.
	    put(truncated_proc,"<0.0.0>")
    end.

is_proc_tag(Tag)  when Tag==?proc;
		       Tag==?proc_dictionary;
		       Tag==?proc_messages;
		       Tag==?debug_proc_dictionary;
		       Tag==?proc_stack;
		       Tag==?proc_heap ->
    true;
is_proc_tag(_) ->
    false.

%%% Inform the crashdump_viewer_server that a background job is completed.
background_done(Result) ->
    Dict = get(),
    cast({background_done,Result,Dict}).    

background_status(Status) ->
    cast({background_status,Status}).

%%%-----------------------------------------------------------------
%%% Functions for reading information from the dump
general_info(File) ->
    [{_Id,Start}] = lookup_index(?erl_crash_dump),
    Fd = open(File),
    pos_bof(Fd,Start),
    Created = case get_rest_of_line(Fd) of
		  {eof,SomeOfLine} -> SomeOfLine;
		  WholeLine -> WholeLine
	      end,

    GI0 = get_general_info(Fd,#general_info{created=Created}),
    GI = case GI0#general_info.num_atoms of
	    ?space -> GI0#general_info{num_atoms=get_num_atoms(Fd)};
	    _ -> GI0
	end,

    {MemTot,MemMax} = 
	case lookup_index(?memory) of
	    [{_,MemStart}] ->
		pos_bof(Fd,MemStart),
		Memory = get_meminfo(Fd,[]),
		Tot = case lists:keysearch("total",1,Memory) of
			  {value,{_,T}} -> T;
			  false -> ""
		      end,
		Max = case lists:keysearch("maximum",1,Memory) of
			  {value,{_,M}} -> M;
			  false -> ""
		      end,
		{Tot,Max};
	    _ ->
		{"",""}
	end,

    close(Fd),
    {NumProcs,NumEts,NumFuns,NumTimers} = count(),
    NodeName = 
	case lookup_index(?node) of
	    [{N,_Start}] ->
		N;
	    [] ->
		case lookup_index(?no_distribution) of
		    [_] -> "nonode@nohost";
		    [] -> "unknown"
		end
	end,

    InstrInfo =
	case lookup_index(?old_instr_data) of
	    [] ->
		case lookup_index(?instr_data) of
		    [] ->
			false;
		    _ ->
			instr_data
		end;
	    _ ->
		old_instr_data
	end,
    GI#general_info{node_name=NodeName,
		    num_procs=integer_to_list(NumProcs),
		    num_ets=integer_to_list(NumEts),
		    num_timers=integer_to_list(NumTimers),
		    num_fun=integer_to_list(NumFuns),
		    mem_tot=MemTot,
		    mem_max=MemMax,
		    instr_info=InstrInfo}.

get_general_info(Fd,GenInfo) ->
    case line_head(Fd) of
	"Slogan" ->
	    get_general_info(Fd,GenInfo#general_info{slogan=val(Fd)});
	"System version" ->
	    get_general_info(Fd,GenInfo#general_info{system_vsn=val(Fd)});
	"Compiled" ->
	    get_general_info(Fd,GenInfo#general_info{compile_time=val(Fd)});
	"Taints" ->
	    Val = case val(Fd) of "-1" -> "(none)"; Line -> Line end,
	    get_general_info(Fd,GenInfo#general_info{taints=Val});
	"Atoms" ->
	    get_general_info(Fd,GenInfo#general_info{num_atoms=val(Fd)});
	"=" ++ _next_tag ->
	    GenInfo;
	Other ->
	    unexpected(Fd,Other,"general information"),
	    GenInfo
    end.

get_num_atoms(Fd) ->
    case lookup_index(?hash_table,"atom_tab") of
	[{_,Pos}] -> 
	    pos_bof(Fd,Pos),
	    skip_rest_of_line(Fd), % size
	    skip_rest_of_line(Fd), % used
	    case line_head(Fd) of
		"objs" ->
		    val(Fd);
		_1 ->
		    get_num_atoms2()
	    end;
	[] ->
	    get_num_atoms2()
    end.
get_num_atoms2() ->
    case lookup_index(?num_atoms) of
	[] -> 
	    ?space;
	[{NA,_Pos}] -> 
	    %% If dump is translated this will exist
	    case get(truncated) of
		true ->
		    [NA," (visible in dump)"]; % might be more
		false ->
		    NA
	    end
    end.

count() ->
    {count_index(?proc),count_index(?ets),count_index(?fu),count_index(?timer)}.


%%-----------------------------------------------------------------
%% Page with all processes
%%
%% If there are less than ?max_sort_process_num processes in the dump,
%% we will store the list of processes in the server state in order to
%% allow sorting according to the different columns of the
%% table. Since ?max_sort_process_num=:=?items_chunk_size, there will
%% never be more than one chunk in this case.
%% 
%% If there are more than ?max_sort_process_num processes in the dump,
%% no sorting will be allowed, and the processes must be read (chunk
%% by chunk) from the file each time the page is opened. This is to
%% avoid really big data in the server state.
procs_summary(SessionId,TW,_,State=#state{procs_summary=too_many}) ->
    chunk_page(SessionId,State#state.file,TW,?proc,processes,
	       {no_sort,State#state.shared_heap,State#state.dump_vsn},
	       procs_summary_parsefun()),
    State;
procs_summary(SessionId,TW,SortOn,State) ->
    ProcsSummary = 
	case State#state.procs_summary of
	    undefined -> % first time - read from file
		Fd = open(State#state.file),
		{PS,_}=lookup_and_parse_index_chunk(first_chunk_pointer(?proc),
						    Fd,procs_summary_parsefun()),
		close(Fd),
		PS;
	    PS ->
		PS
	end,
    {SortedPS,NewSorted} = do_sort_procs(SortOn,ProcsSummary,State),
    HtmlInfo = 
	crashdump_viewer_html:chunk_page(processes,SessionId,TW,
					 {SortOn,
					  State#state.shared_heap,
					  State#state.dump_vsn},
					 SortedPS),
    crashdump_viewer_html:chunk(SessionId,done,HtmlInfo),
    State#state{procs_summary=ProcsSummary,sorted=NewSorted}.

procs_summary_parsefun() ->
    fun(Fd,Pid) -> 
	    get_procinfo(Fd,fun main_procinfo/4,#proc{pid=Pid}) 
    end.

%%-----------------------------------------------------------------
%% Page with one process
get_proc_details(File,Pid,DumpVsn) ->
    case lookup_index(?proc,Pid) of
	[{_,Start}] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    Proc0 = 
		case DumpVsn of
		    [0,0] ->
			%% Old version (translated)
			#proc{pid=Pid};
		    _ ->
			#proc{pid=Pid,
			      stack_dump=if_exist(?proc_stack,Pid),
			      msg_q=if_exist(?proc_messages,Pid),
			      dict=if_exist(?proc_dictionary,Pid),
			      debug_dict=if_exist(?debug_proc_dictionary,Pid)}
		end,
	    Proc = get_procinfo(Fd,fun all_procinfo/4,Proc0),
	    close(Fd),
	    {ok,Proc};
	_ ->
	    case maybe_other_node(File,Pid) of
		{other_node,Type,Node} -> 
		    Info = "The process you are searching for was residing on "
			"a remote node. No process information is available. "
			"Information about the remote node is show below.",
		    {other_node,{Type,Info,Node}};
		not_found ->
		    not_found
	    end
    end.

if_exist(Tag,Key) ->
    case count_index(Tag,Key) of
	0 -> 
	    Tag1 = 
		case is_proc_tag(Tag) of
		    true -> ?proc;
		    false -> Tag
		end,
	    case truncated_here({Tag1,Key}) of
		true -> truncated;
		false -> ?space
	    end;
	_ -> 
	    expand
    end.

get_procinfo(Fd,Fun,Proc) ->
    case line_head(Fd) of
	"State" ->
	    State = case val(Fd) of
			"Garbing" -> "Garbing\n(limited info)";
			State0 -> State0
		    end,
	    get_procinfo(Fd,Fun,Proc#proc{state=State});
	"Name" ->
	    get_procinfo(Fd,Fun,Proc#proc{name=val(Fd)});
	"Spawned as" ->
	    IF = val(Fd),
	    case Proc#proc.name of
		?space ->
		    get_procinfo(Fd,Fun,Proc#proc{name=IF,init_func=IF});
		_ ->
		    get_procinfo(Fd,Fun,Proc#proc{init_func=IF})
	    end;
	"Spawned by" ->
	    case val(Fd) of
		"[]" ->
		    get_procinfo(Fd,Fun,Proc);
		Parent ->
		    get_procinfo(Fd,Fun,Proc#proc{parent=Parent})
	    end;
	"Started" ->
	    get_procinfo(Fd,Fun,Proc#proc{start_time=val(Fd)});
	"Last scheduled in for" ->
	    get_procinfo(Fd,Fun,Proc#proc{current_func=
					  {"Last scheduled in for",
					   val(Fd)}});
	"Current call" ->
	    get_procinfo(Fd,Fun,Proc#proc{current_func={"Current call",
							val(Fd)}});
	"Message queue length" -> 
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{msg_q_len=list_to_integer(val(Fd))});
	"Reductions" ->
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{reds=list_to_integer(val(Fd))});
	"Number of heap fragments" ->
	    get_procinfo(Fd,Fun,Proc#proc{num_heap_frag=val(Fd)});
	"Heap fragment data" ->
	    get_procinfo(Fd,Fun,Proc#proc{heap_frag_data=val(Fd)});
	Stack when Stack=:="Stack+heap"; Stack=:="Stack" ->
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{stack_heap=
					  list_to_integer(val(Fd))});
	"OldHeap" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap=val(Fd)});
	"Heap unused" ->
	    get_procinfo(Fd,Fun,Proc#proc{heap_unused=val(Fd)});
	"OldHeap unused" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_unused=val(Fd)});
	"New heap start" ->
	    get_procinfo(Fd,Fun,Proc#proc{new_heap_start=val(Fd)});
	"New heap top" ->
	    get_procinfo(Fd,Fun,Proc#proc{new_heap_top=val(Fd)});
	"Stack top" ->
	    get_procinfo(Fd,Fun,Proc#proc{stack_top=val(Fd)});
	"Stack end" ->
	    get_procinfo(Fd,Fun,Proc#proc{stack_end=val(Fd)});
	"Old heap start" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_start=val(Fd)});
	"Old heap top" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_top=val(Fd)});
	"Old heap end" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_end=val(Fd)});
	"Memory" ->
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{memory=list_to_integer(val(Fd))});
	{eof,_} ->
	    Proc; % truncated file
	Other ->
	    Fun(Fd,Fun,Proc,Other)
    end.

main_procinfo(Fd,Fun,Proc,LineHead) ->
    case LineHead of
	"Stack dump" ->
	    %% This is the last element in older dumps (DumpVsn=0.0)
	    Proc;
	"=" ++ _next_tag ->
	    %% DumpVsn=0.1 or newer: No stack dump here
	    Proc;
	"arity = " ++ _ ->
	    %%! Temporary workaround
	    get_procinfo(Fd,Fun,Proc);	
	_Other ->
	    skip_rest_of_line(Fd),
	    get_procinfo(Fd,Fun,Proc)
    end.
all_procinfo(Fd,Fun,Proc,LineHead) ->
    case LineHead of
	"Message queue" -> 
	    get_procinfo(Fd,Fun,Proc#proc{msg_q=size_or_term(Fd)});
	"Last calls" ->
	    R = case size_or_term(Fd) of
		    SizeThing when is_tuple(SizeThing) ->
			Proc#proc{last_calls=SizeThing};
		    Term ->
			Proc#proc{last_calls=replace_all($ ,$\n,Term,[])}
		end,
	    get_procinfo(Fd,Fun,R);
	"Link list" ->
	    get_procinfo(Fd,Fun,Proc#proc{links=val(Fd)});
	"Program counter" ->
	    get_procinfo(Fd,Fun,Proc#proc{prog_count=val(Fd)});
	"CP" ->
	    get_procinfo(Fd,Fun,Proc#proc{cp=val(Fd)});
	"arity = " ++ Arity ->
	    %%! Temporary workaround
	    get_procinfo(Fd,Fun,Proc#proc{arity=Arity--"\r\n"});
	"Dictionary" ->
	    get_procinfo(Fd,Fun,Proc#proc{dict=size_or_term(Fd)});
	"$Dictionary" ->
	    get_procinfo(Fd,Fun,Proc#proc{debug_dict=size_or_term(Fd)});
	"Stack dump" ->
	    %% This is the last element in older dumps (DumpVsn=0.0)
	    get_stack_dump(Fd,Proc);
	"=" ++ _next_tag ->
	    %% DumpVsn=0.1 or newer: No stack dump here
	    Proc;
	Other ->
	    unexpected(Fd,Other,"process info"),
	    get_procinfo(Fd,Fun,Proc)
    end.

get_stack_dump(Fd,Proc) ->
    %% Always show stackdump as "Expand" link
    Pos = get(pos),
    Size = count_rest_of_tag(Fd),
    Proc#proc{stack_dump={size,true,Size,Pos}}.

maybe_other_node(File,Id) ->
    Channel = 
	case split($.,Id) of
	    {"<" ++ N, _Rest} ->
		N;
	    {"#Port<" ++ N, _Rest} ->
		N
	end,
    Ms = ets:fun2ms(
	   fun({{Tag,Start},Ch}) when Tag=:=?visible_node, Ch=:=Channel -> 
		   {"Visible Node",Start};
	      ({{Tag,Start},Ch}) when Tag=:=?hidden_node, Ch=:=Channel ->
		   {"Hidden Node",Start};
	      ({{Tag,Start},Ch}) when Tag=:=?not_connected, Ch=:=Channel -> 
		   {"Not Connected Node",Start}
	   end),
    
    case ets:select(cdv_dump_index_table,Ms) of
	[] -> 
	    not_found;
	[{Type,Pos}] -> 
	    Fd = open(File),
	    NodeInfo = get_nodeinfo(Fd,Channel,Pos),
	    close(Fd),
	    {other_node,Type,NodeInfo}
    end.


expand_memory(File,What,Pid,Binaries) ->
    Fd = open(File),
    put(fd,Fd),
    Dict = read_heap(Fd,Pid,Binaries),
    Expanded = 
	case What of
	    "StackDump" -> read_stack_dump(Fd,Pid,Dict);
	    "MsgQueue" -> read_messages(Fd,Pid,Dict);
	    "Dictionary" -> read_dictionary(Fd,?proc_dictionary,Pid,Dict);
	    "DebugDictionary" -> read_dictionary(Fd,?debug_proc_dictionary,Pid,Dict)
	end,
    erase(fd),
    close(Fd),
    Expanded.
    
%%%
%%% Read binaries.
%%%
read_binaries(Fd) ->
    AllBinaries = lookup_index(?binary),
    read_binaries(Fd,AllBinaries, gb_trees:empty()).

read_binaries(Fd,[{Addr0,Pos}|Bins],Dict0) ->
    pos_bof(Fd,Pos),
    {Addr,_} = get_hex(Addr0),
    Dict = 
	case line_head(Fd) of
	    {eof,_} ->
		gb_trees:enter(Addr,'#CDVTruncatedBinary',Dict0);
	    Size0 ->
		{Size,_} = get_hex(Size0),
		if Size > ?max_display_binary_size ->
			gb_trees:enter(Addr,{'#CDVTooBig',binary,Pos},Dict0);
		   true ->
			pos_bof(Fd,Pos),
			Line = val(Fd),
			parse_binary(Addr,Line,Dict0)
		end
	end,
    read_binaries(Fd,Bins,Dict);
read_binaries(_Fd,[],Dict) ->
    Dict.

parse_binary(Addr, Line0, Dict) ->
    case get_hex(Line0) of
	{N,":"++Line1} ->
	    {Bin,Line} = get_binary(N, Line1, []),
	    [] = skip_blanks(Line),
	    gb_trees:enter(Addr, Bin, Dict);
	{_N,[]} ->
	    %% If the dump is truncated before the ':' in this line, then
	    %% line_head/1 might not discover it (if a \n has been inserted
	    %% somehow???)
	    gb_trees:enter(Addr,'#CDVTruncatedBinary',Dict)
    end.



%%%
%%% Read top level section.
%%%

read_stack_dump(Fd,Pid,Dict) ->
    case lookup_index(?proc_stack,Pid) of
	[{_,Start}] ->
	    pos_bof(Fd,Start),
	    read_stack_dump1(Fd,Dict,[]);
	[] ->
	    []
    end.
read_stack_dump1(Fd,Dict,Acc) ->
    %% This function is never called if the dump is truncated in {?proc_heap,Pid}
    case val(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	Line ->
	    Stack = parse_top(Line,Dict),
	    read_stack_dump1(Fd,Dict,[Stack|Acc])
    end.

parse_top(Line0, D) ->
    {Label,Line1} = get_label(Line0),
    {Term,Line,D} = parse_term(Line1, D),
    [] = skip_blanks(Line),
    {Label,Term}.

%%%
%%% Read message queue.
%%%

read_messages(Fd,Pid,Dict) ->
    case lookup_index(?proc_messages,Pid) of
	[{_,Start}] ->
	    pos_bof(Fd,Start),
	    read_messages1(Fd,Dict,[]);
	[] ->
	    []
    end.
read_messages1(Fd,Dict,Acc) ->
    %% This function is never called if the dump is truncated in {?proc_heap,Pid}
    case val(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	Line ->
	    Msg = parse_message(Line,Dict),
	    read_messages1(Fd,Dict,[Msg|Acc])
    end.
    
parse_message(Line0, D) ->
    {Msg,":"++Line1,_} = parse_term(Line0, D),
    {Token,Line,_} = parse_term(Line1, D),
    [] = skip_blanks(Line),
    {Msg,Token}.
    
%%%
%%% Read process dictionary
%%%

read_dictionary(Fd,Tag,Pid,Dict) ->
    case lookup_index(Tag,Pid) of
	[{_,Start}] ->
	    pos_bof(Fd,Start),
	    read_dictionary1(Fd,Dict,[]);
	[] ->
	    []
    end.
read_dictionary1(Fd,Dict,Acc) ->
    %% This function is never called if the dump is truncated in {?proc_heap,Pid}
    case val(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	Line ->
	    Msg = parse_dictionary(Line,Dict),
	    read_dictionary1(Fd,Dict,[Msg|Acc])
    end.
    
parse_dictionary(Line0, D) ->
    {Entry,Line,_} = parse_term(Line0, D),
    [] = skip_blanks(Line),
    Entry.
    
%%%
%%% Read heap data.
%%%

read_heap(Fd,Pid,Dict0) ->
    case lookup_index(?proc_heap,Pid) of
	[{_,Pos}] ->
	    pos_bof(Fd,Pos),
	    read_heap(Dict0);
	[] ->
	    Dict0
    end.

read_heap(Dict0) ->
    %% This function is never called if the dump is truncated in {?proc_heap,Pid}
    case get(fd) of
	end_of_heap ->
	    Dict0;
	Fd ->
	    case val(Fd) of
		"=" ++ _next_tag ->
		    put(fd, end_of_heap),
		    Dict0;
		Line ->
		    Dict = parse(Line,Dict0),
		    read_heap(Dict)
	    end
    end.

parse(Line0, Dict0) ->
    {Addr,":"++Line1} = get_hex(Line0),
    {_Term,Line,Dict} = parse_heap_term(Line1, Addr, Dict0),
    [] = skip_blanks(Line),
    Dict.


do_sort_procs("state",Procs,#state{sorted="state"}) ->
    {lists:reverse(lists:keysort(#proc.state,Procs)),"rstate"};
do_sort_procs("state",Procs,_) ->
    {lists:keysort(#proc.state,Procs),"state"};
do_sort_procs("pid",Procs,#state{sorted="pid"}) ->
    {lists:reverse(Procs),"rpid"};
do_sort_procs("pid",Procs,_) ->
    {Procs,"pid"};
do_sort_procs("msg_q_len",Procs,#state{sorted="msg_q_len"}) ->
    {lists:keysort(#proc.msg_q_len,Procs),"rmsg_q_len"};
do_sort_procs("msg_q_len",Procs,_) ->
    {lists:reverse(lists:keysort(#proc.msg_q_len,Procs)),"msg_q_len"};
do_sort_procs("reds",Procs,#state{sorted="reds"}) ->
    {lists:keysort(#proc.reds,Procs),"rreds"};
do_sort_procs("reds",Procs,_) ->
    {lists:reverse(lists:keysort(#proc.reds,Procs)),"reds"};
do_sort_procs("mem",Procs,#state{sorted="mem",dump_vsn=DumpVsn}) ->
    KeyPos = if DumpVsn>=?r16b01_dump_vsn -> #proc.memory;
		true -> #proc.stack_heap
	     end,
    {lists:keysort(KeyPos,Procs),"rmem"};
do_sort_procs("mem",Procs,#state{dump_vsn=DumpVsn}) ->
    KeyPos = if DumpVsn>=?r16b01_dump_vsn -> #proc.memory;
		true -> #proc.stack_heap
	     end,
    {lists:reverse(lists:keysort(KeyPos,Procs)),"mem"};
do_sort_procs("init_func",Procs,#state{sorted="init_func"}) ->
    {lists:reverse(lists:keysort(#proc.init_func,Procs)),"rinit_func"};
do_sort_procs("init_func",Procs,_) ->
    {lists:keysort(#proc.init_func,Procs),"init_func"};
do_sort_procs("name_func",Procs,#state{sorted="name_func"}) ->
    {lists:reverse(lists:keysort(#proc.name,Procs)),"rname_func"};
do_sort_procs("name_func",Procs,_) ->
    {lists:keysort(#proc.name,Procs),"name_func"};
do_sort_procs("name",Procs,#state{sorted=Sorted}) ->
    {No,Yes} = 
	lists:foldl(fun(P,{N,Y}) ->
			    case P#proc.name of
				?space -> {[P|N],Y};
				_other -> {N,[P|Y]}
			    end
		    end,
		    {[],[]},
		    Procs),
    Result = lists:keysort(#proc.name,Yes) ++ No,
    case Sorted of
	"name" -> {lists:reverse(Result),"rname"};
	_ -> {Result,"name"}
    end.
    
%%-----------------------------------------------------------------
%% Page with one port
get_port(File,Port) ->
    case lookup_index(?port,Port) of
	[{_,Start}] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    R = get_portinfo(Fd,#port{id=Port}),
	    close(Fd),
	    {ok,R};
	[] ->
	    case maybe_other_node(File,Port) of
		{other_node,Type,Node} -> 
		    Info = "The port you are searching for was residing on "
			"a remote node. No port information is available. "
			"Information about the remote node is show below.",
		    {other_node,{Type,Info,Node}};
		not_found ->
		    not_found
	    end
    end.

%%-----------------------------------------------------------------
%% Page with all ports
get_ports(SessionId,File,TW) ->
    ParseFun = fun(Fd,Id) -> get_portinfo(Fd,#port{id=Id}) end,
    chunk_page(SessionId,File,TW,?port,ports,[],ParseFun).

get_portinfo(Fd,Port) ->
    case line_head(Fd) of
	"Slot" ->
	    get_portinfo(Fd,Port#port{slot=val(Fd)});
	"Connected" ->
	    get_portinfo(Fd,Port#port{connected=val(Fd)});
	"Links" ->
	    get_portinfo(Fd,Port#port{links=val(Fd)});
	"Registered as" ->
	    get_portinfo(Fd,Port#port{name=val(Fd)});
	"Monitors" ->
	    get_portinfo(Fd,Port#port{monitors=val(Fd)});
	"Port controls linked-in driver" ->
	    get_portinfo(Fd,Port#port{controls=["Linked in driver: " |
						val(Fd)]});
	"Port controls external process" ->
	    get_portinfo(Fd,Port#port{controls=["External proc: " | val(Fd)]});
	"Port is a file" ->
	    get_portinfo(Fd,Port#port{controls=["File: "| val(Fd)]});
	"Port is UNIX fd not opened by emulator" ->
	    get_portinfo(Fd,Port#port{
			      controls=["UNIX fd not opened by emulator: "| 
					val(Fd)]});
	"=" ++ _next_tag ->
	    Port;
	Other ->
	    unexpected(Fd,Other,"port info"),
	    Port
    end.


%%-----------------------------------------------------------------
%% Page with external ets tables
get_ets_tables(SessionId,File,Heading,TW,Pid,WS) ->
    ParseFun = fun(Fd,Id) -> get_etsinfo(Fd,#ets_table{pid=Id},WS) end,
    chunk_page(SessionId,File,TW,{?ets,Pid},ets_tables,Heading,ParseFun).

get_etsinfo(Fd,EtsTable,WS) ->
    case line_head(Fd) of
	"Slot" ->
	    get_etsinfo(Fd,EtsTable#ets_table{slot=val(Fd)},WS);
	"Table" ->
	    get_etsinfo(Fd,EtsTable#ets_table{id=val(Fd)},WS);
	"Name" ->
	    get_etsinfo(Fd,EtsTable#ets_table{name=val(Fd)},WS);
	"Ordered set (AVL tree), Elements" ->
	    skip_rest_of_line(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{type="tree",buckets="-"},WS);
	"Buckets" ->
	    get_etsinfo(Fd,EtsTable#ets_table{buckets=val(Fd)},WS);
	"Objects" ->
	    get_etsinfo(Fd,EtsTable#ets_table{size=val(Fd)},WS);
	"Words" ->
	    Words = list_to_integer(val(Fd)),
	    Bytes = 
		case Words of
		    -1 -> "-1"; % probably truncated
		    _ -> integer_to_list(Words * WS)
		end,
	    get_etsinfo(Fd,EtsTable#ets_table{memory=Bytes},WS);
	"=" ++ _next_tag ->
	    EtsTable;
	Other ->
	    unexpected(Fd,Other,"ETS info"),
	    EtsTable
    end.


%% Internal ets table page
get_internal_ets_tables(File,WS) ->
    InternalEts = lookup_index(?internal_ets),
    Fd = open(File),
    R = lists:map(
	  fun({Descr,Start}) ->
		  pos_bof(Fd,Start),
		  {Descr,get_etsinfo(Fd,#ets_table{},WS)}
	  end,
	  InternalEts),
    close(Fd),
    R.

%%-----------------------------------------------------------------
%% Page with list of all timers 
get_timers(SessionId,File,Heading,TW,Pid) ->
    ParseFun = fun(Fd,Id) -> get_timerinfo_1(Fd,#timer{pid=Id}) end,
    chunk_page(SessionId,File,TW,{?timer,Pid},timers,Heading,ParseFun).

get_timerinfo_1(Fd,Timer) ->
    case line_head(Fd) of
	"Message" ->
	    get_timerinfo_1(Fd,Timer#timer{msg=val(Fd)});
	"Time left" ->
	    get_timerinfo_1(Fd,Timer#timer{time=val(Fd)});
	"=" ++ _next_tag ->
	    Timer;
	Other ->
	    unexpected(Fd,Other,"timer info"),
	    Timer
    end.

%%-----------------------------------------------------------------
%% Page with information about the erlang distribution
nods(File) ->
    case lookup_index(?no_distribution) of
	[] ->
	    V = lookup_index(?visible_node),
	    H = lookup_index(?hidden_node),
	    N = lookup_index(?not_connected),
	    Fd = open(File),
	    Visible = lists:map(
			fun({Channel,Start}) -> 
				get_nodeinfo(Fd,Channel,Start)
			end, 
			V),
	    Hidden = lists:map(
		       fun({Channel,Start}) -> 
			       get_nodeinfo(Fd,Channel,Start)
		       end, 
		       H),
	    NotConnected = lists:map(
			     fun({Channel,Start}) -> 
				     get_nodeinfo(Fd,Channel,Start)
			     end, 
			     N),
	    close(Fd),
	    {Visible,Hidden,NotConnected};
	[_] ->
	    no_distribution
    end.

get_nodeinfo(Fd,Channel,Start) ->
    pos_bof(Fd,Start),
    get_nodeinfo(Fd,#nod{channel=Channel}).

get_nodeinfo(Fd,Nod) ->
    case line_head(Fd) of
	"Name" ->
	    get_nodeinfo(Fd,Nod#nod{name=val(Fd)});
	"Controller" ->
	    get_nodeinfo(Fd,Nod#nod{controller=val(Fd)});
	"Creation" ->
	    get_nodeinfo(Fd,Nod#nod{creation=val(Fd)});
	"Remote link" ->
	    Procs = val(Fd), % e.g. "<0.31.0> <4322.54.0>"
	    RemoteLinks = Nod#nod.remote_links,
	    get_nodeinfo(Fd,Nod#nod{remote_links=[split(Procs)|RemoteLinks]});
	"Remote monitoring" ->
	    Procs = val(Fd), % e.g. "<0.31.0> <4322.54.0>"
	    RemoteMon = Nod#nod.remote_mon,
	    get_nodeinfo(Fd,Nod#nod{remote_mon=[split(Procs)|RemoteMon]});
	"Remotely monitored by" ->
	    Procs = val(Fd), % e.g. "<0.31.0> <4322.54.0>"
	    RemoteMonBy = Nod#nod.remote_mon_by,
	    get_nodeinfo(Fd,Nod#nod{remote_mon_by=[split(Procs)|RemoteMonBy]});
	"Error" ->
	    get_nodeinfo(Fd,Nod#nod{error=val(Fd)});	    
	"=" ++ _next_tag ->
	    Nod;
	Other ->
	    unexpected(Fd,Other,"node info"),
	    Nod
    end.

%%-----------------------------------------------------------------
%% Page with details about one loaded modules
get_loaded_mod_details(File,Mod) ->
    [{_,Start}] = lookup_index(?mod,Mod),
    Fd = open(File),
    pos_bof(Fd,Start),
    InitLM = #loaded_mod{mod=Mod,old_size="No old code exists"},
    ModInfo = get_loaded_mod_info(Fd,InitLM,fun all_modinfo/3),
    close(Fd),
    ModInfo.

%%-----------------------------------------------------------------
%% Page with list of all loaded modules
loaded_mods(SessionId,File,TW) ->
    ParseFun = 
	fun(Fd,Id) -> 
		get_loaded_mod_info(Fd,#loaded_mod{mod=Id},
				    fun main_modinfo/3) 
	end,
    {CC,OC} = 
	case lookup_index(?loaded_modules) of
	    [{_,StartTotal}] ->
		Fd = open(File),
		pos_bof(Fd,StartTotal),
		R = get_loaded_mod_totals(Fd,{"unknown","unknown"}),
		close(Fd),
		R;
	    [] ->
		{"unknown","unknown"}
    end,
    chunk_page(SessionId,File,TW,?mod,loaded_mods,{CC,OC},ParseFun).

get_loaded_mod_totals(Fd,{CC,OC}) ->
    case line_head(Fd) of
	"Current code" ->
	    get_loaded_mod_totals(Fd,{val(Fd),OC});
	"Old code" ->
	    get_loaded_mod_totals(Fd,{CC,val(Fd)});
	"=" ++ _next_tag ->
	    {CC,OC};
	Other ->
	    unexpected(Fd,Other,"loaded modules info"),
	    {CC,OC} % truncated file
    end.

get_loaded_mod_info(Fd,LM,Fun) ->
    case line_head(Fd) of
	"Current size" ->
	    get_loaded_mod_info(Fd,LM#loaded_mod{current_size=val(Fd)},Fun);
	"Old size" ->
	    get_loaded_mod_info(Fd,LM#loaded_mod{old_size=val(Fd)},Fun);
	"=" ++ _next_tag ->
	    LM;
	{eof,_} ->
	    LM; % truncated file
	Other ->
	    LM1 = Fun(Fd,LM,Other),
	    get_loaded_mod_info(Fd,LM1,Fun)
    end.

main_modinfo(_Fd,LM,_LineHead) ->
    LM.
all_modinfo(Fd,LM,LineHead) ->
    case LineHead of
	"Current attributes" ->
	    Str = hex_to_str(val(Fd)),
	    LM#loaded_mod{current_attrib=Str};
	"Current compilation info" ->
	    Str = hex_to_str(val(Fd)),
	    LM#loaded_mod{current_comp_info=Str};
	"Old attributes" ->
	    Str = hex_to_str(val(Fd)),
	    LM#loaded_mod{old_attrib=Str};
	"Old compilation info" ->
	    Str = hex_to_str(val(Fd)),
	    LM#loaded_mod{old_comp_info=Str};
	Other ->
	    unexpected(Fd,Other,"loaded modules info"),
	    LM
    end.
    

hex_to_str(Hex) ->
    Term = hex_to_term(Hex,[]),
    io_lib:format("~p~n",[Term]).

hex_to_term([X,Y|Hex],Acc) ->
    MS = hex_to_dec([X]),
    LS = hex_to_dec([Y]),
    Z = 16*MS+LS,
    hex_to_term(Hex,[Z|Acc]);
hex_to_term([],Acc) ->
    Bin = list_to_binary(lists:reverse(Acc)),
    case catch binary_to_term(Bin) of
	{'EXIT',_Reason} ->
	    {"WARNING: The term is probably truncated!",
	     "I can not do binary_to_term.",
	     Bin};
	Term ->
	    Term
    end.

hex_to_dec("F") -> 15;
hex_to_dec("E") -> 14;
hex_to_dec("D") -> 13;
hex_to_dec("C") -> 12;
hex_to_dec("B") -> 11;
hex_to_dec("A") -> 10;
hex_to_dec(N) -> list_to_integer(N).
    

%%-----------------------------------------------------------------
%% Page with list of all funs
funs(SessionId,File,TW) ->
    ParseFun = fun(Fd,_Id) -> get_funinfo(Fd,#fu{}) end,
    chunk_page(SessionId,File,TW,?fu,funs,[],ParseFun).

get_funinfo(Fd,Fu) ->
    case line_head(Fd) of
	"Module" ->
	    get_funinfo(Fd,Fu#fu{module=val(Fd)});
	"Uniq" ->
	    get_funinfo(Fd,Fu#fu{uniq=val(Fd)});
	"Index" ->
	    get_funinfo(Fd,Fu#fu{index=val(Fd)});
	"Address" ->
	    get_funinfo(Fd,Fu#fu{address=val(Fd)});
	"Native_address" ->
	    get_funinfo(Fd,Fu#fu{native_address=val(Fd)});
	"Refc" ->
	    get_funinfo(Fd,Fu#fu{refc=val(Fd)});
	"=" ++ _next_tag ->
	    Fu;
	Other ->
	    unexpected(Fd,Other,"fun info"),
	    Fu
    end.

%%-----------------------------------------------------------------
%% Page with list of all atoms
atoms(SessionId,File,TW,Num) ->
    case lookup_index(?atoms) of
	[{_Id,Start}] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    case get_atoms(Fd,?items_chunk_size) of
		{Atoms,Cont} ->
		    crashdump_viewer_html:atoms(SessionId,TW,Num,Atoms),
		    atoms_chunks(Fd,SessionId,Cont);
		done ->
		    crashdump_viewer_html:atoms(SessionId,TW,Num,done)
	    end;
	_ ->
	    crashdump_viewer_html:atoms(SessionId,TW,Num,done)
    end.

get_atoms(Fd,Number) ->
    case get_n_lines_of_tag(Fd,Number) of
	{all,_,Lines} ->
	    close(Fd),
	    {Lines,done};
	{part,_,Lines} ->
	    {Lines,Number};
	empty ->
	    close(Fd),
	    done
    end.

atoms_chunks(_Fd,SessionId,done) ->
    crashdump_viewer_html:atoms_chunk(SessionId,done);
atoms_chunks(Fd,SessionId,Number) ->
    case get_atoms(Fd,Number) of
	{Atoms,Cont} ->
	    crashdump_viewer_html:atoms_chunk(SessionId,Atoms),
	    atoms_chunks(Fd,SessionId,Cont);
	done ->
	    atoms_chunks(Fd,SessionId,done)
    end.


%%-----------------------------------------------------------------
%% Page with memory information
memory(File) ->
    case lookup_index(?memory) of
	[{_,Start}] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    R = get_meminfo(Fd,[]),
	    close(Fd),
	    R;
	_ ->
	    []
    end.

get_meminfo(Fd,Acc) ->
    case line_head(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	{eof,_last_line} ->
	    lists:reverse(Acc);
	Key ->
	    get_meminfo(Fd,[{Key,val(Fd)}|Acc])
    end.

%%-----------------------------------------------------------------
%% Page with information about allocated areas
allocated_areas(File) ->
    case lookup_index(?allocated_areas) of
	[{_,Start}] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    R = get_allocareainfo(Fd,[]),
	    close(Fd),
	    R;
	_ ->
	    []
    end.

get_allocareainfo(Fd,Acc) ->
    case line_head(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	{eof,_last_line} ->
	    lists:reverse(Acc);
	Key ->
	    Val = val(Fd),
	    AllocInfo =
		case split(Val) of
		    {Alloc,[]} ->
			{Key,Alloc,?space};
		    {Alloc,Used} ->
			{Key,Alloc,Used}
		end,
	    get_allocareainfo(Fd,[AllocInfo|Acc])
    end.

%%-----------------------------------------------------------------
%% Page with information about allocators
allocator_info(File) ->
    case lookup_index(?allocator) of
	[] ->
	    [];
	AllAllocators ->
	    Fd = open(File),
	    R = lists:map(fun({Heading,Start}) ->
				  {Heading,get_allocatorinfo(Fd,Start)} 
			  end, 
			  AllAllocators),
	    close(Fd),
	    [allocator_summary(R) | R]
    end.

get_allocatorinfo(Fd,Start) ->
    pos_bof(Fd,Start),
    get_allocatorinfo1(Fd,[]).

get_allocatorinfo1(Fd,Acc) ->
    case line_head(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	{eof,_last_line} ->
	    lists:reverse(Acc);
	Key ->
	    Values = get_all_vals(val(Fd),[]),
	    get_allocatorinfo1(Fd,[{Key,Values}|Acc])
    end.
	    
get_all_vals([$ |Rest],Acc) ->
    [lists:reverse(Acc)|get_all_vals(Rest,[])];
get_all_vals([],Acc) ->
    [lists:reverse(Acc)];
get_all_vals([Char|Rest],Acc) ->
    get_all_vals(Rest,[Char|Acc]).

%% Calculate allocator summary:
%%
%% System totals:
%%   blocks size   = sum of mbcs, mbcs_pool and sbcs blocks size over
%%                   all allocator instances of all types
%%   carriers size = sum of mbcs, mbcs_pool and sbcs carriers size over
%%                   all allocator instances of all types
%%
%% I any allocator except sbmbc_alloc has "option e: false" then don't
%% present system totals.
%%
%% For each allocator type:
%%   blocks size        = sum of sbmbcs, mbcs, mbcs_pool and sbcs blocks
%%                        size over all allocator instances of this type
%%   carriers size      = sum of sbmbcs, mbcs, mbcs_pool and sbcs carriers
%%                        size over all allocator instances of this type
%%   mseg carriers size = sum of mbcs and sbcs mseg carriers size over all
%%                        allocator instances of this type
%%

-define(sbmbcs_blocks_size,"sbmbcs blocks size").
-define(mbcs_blocks_size,"mbcs blocks size").
-define(sbcs_blocks_size,"sbcs blocks size").
-define(sbmbcs_carriers_size,"sbmbcs carriers size").
-define(mbcs_carriers_size,"mbcs carriers size").
-define(sbcs_carriers_size,"sbcs carriers size").
-define(mbcs_mseg_carriers_size,"mbcs mseg carriers size").
-define(sbcs_mseg_carriers_size,"sbcs mseg carriers size").
-define(segments_size,"segments_size").
-define(mbcs_pool_blocks_size,"mbcs_pool blocks size").
-define(mbcs_pool_carriers_size,"mbcs_pool carriers size").

-define(type_blocks_size,[?sbmbcs_blocks_size,
			  ?mbcs_blocks_size,
			  ?mbcs_pool_blocks_size,
			  ?sbcs_blocks_size]).
-define(type_carriers_size,[?sbmbcs_carriers_size,
			    ?mbcs_carriers_size,
			    ?mbcs_pool_carriers_size,
			    ?sbcs_carriers_size]).
-define(type_mseg_carriers_size,[?mbcs_mseg_carriers_size,
				 ?sbcs_mseg_carriers_size]).
-define(total_blocks_size,[?mbcs_blocks_size,
			   ?mbcs_pool_blocks_size,
			   ?sbcs_blocks_size]).
-define(total_carriers_size,[?mbcs_carriers_size,
			     ?mbcs_pool_carriers_size,
			     ?sbcs_carriers_size]).
-define(total_mseg_carriers_size,[?mbcs_mseg_carriers_size,
				  ?sbcs_mseg_carriers_size]).
-define(interesting_allocator_info, [?sbmbcs_blocks_size,
				     ?mbcs_blocks_size,
				     ?mbcs_pool_blocks_size,
				     ?sbcs_blocks_size,
				     ?sbmbcs_carriers_size,
				     ?mbcs_carriers_size,
				     ?sbcs_carriers_size,
				     ?mbcs_mseg_carriers_size,
				     ?mbcs_pool_carriers_size,
				     ?sbcs_mseg_carriers_size,
				     ?segments_size]).
-define(mseg_alloc,"mseg_alloc").
-define(seg_size,"segments_size").
-define(sbmbc_alloc,"sbmbc_alloc").
-define(opt_e_false,{"option e","false"}).

allocator_summary(Allocators) ->
    {Sorted,DoTotal} = sort_allocator_types(Allocators,[],true),
    {TypeTotals0,Totals} = sum_allocator_data(Sorted,DoTotal),
    {TotalMCS,TypeTotals} =
	case lists:keytake(?mseg_alloc,1,TypeTotals0) of
	    {value,{_,[{?seg_size,SegSize}]},Rest} ->
		{integer_to_list(SegSize),Rest};
	    false ->
		{?not_available,TypeTotals0}
	end,
    {TotalBS,TotalCS} =
	case Totals of
	    false ->
		{?not_available,?not_available};
	    {TBS,TCS} ->
		{integer_to_list(TBS),integer_to_list(TCS)}
	end,
    {{"Summary",["blocks size","carriers size","mseg carriers size"]},
     [{"total",[TotalBS,TotalCS,TotalMCS]} |
      format_allocator_summary(lists:reverse(TypeTotals))]}.

format_allocator_summary([{Type,Data}|Rest]) ->
    [format_allocator_summary(Type,Data) | format_allocator_summary(Rest)];
format_allocator_summary([]) ->
    [].

format_allocator_summary(Type,Data) ->
    BS = get_size_value(blocks_size,Data),
    CS = get_size_value(carriers_size,Data),
    MCS = get_size_value(mseg_carriers_size,Data),
    {Type,[BS,CS,MCS]}.

get_size_value(Key,Data) ->
    case proplists:get_value(Key,Data) of
	undefined ->
	    ?not_available;
	Int ->
	    integer_to_list(Int)
    end.

%% Sort allocator data per type
%%  Input  = [{Instance,[{Key,Data}]}]
%%  Output = [{Type,[{Key,Value}]}]
%% where Key in Output is one of ?interesting_allocator_info
%% and Value is the sum over all allocator instances of each type.
sort_allocator_types([{Name,Data}|Allocators],Acc,DoTotal) ->
    Type =
	case string:tokens(Name,"[]") of
	    [T,_Id] -> T;
	    [Name] -> Name
	end,
    TypeData = proplists:get_value(Type,Acc,[]),
    {NewTypeData,NewDoTotal} = sort_type_data(Type,Data,TypeData,DoTotal),
    NewAcc = lists:keystore(Type,1,Acc,{Type,NewTypeData}),
    sort_allocator_types(Allocators,NewAcc,NewDoTotal);
sort_allocator_types([],Acc,DoTotal) ->
    {Acc,DoTotal}.

sort_type_data(Type,[?opt_e_false|Data],Acc,_) when Type=/=?sbmbc_alloc->
    sort_type_data(Type,Data,Acc,false);
sort_type_data(Type,[{Key,Val0}|Data],Acc,DoTotal) ->
    case lists:member(Key,?interesting_allocator_info) of
	true ->
	    Val = list_to_integer(hd(Val0)),
	    sort_type_data(Type,Data,update_value(Key,Val,Acc),DoTotal);
	false ->
	    sort_type_data(Type,Data,Acc,DoTotal)
    end;
sort_type_data(_Type,[],Acc,DoTotal) ->
    {Acc,DoTotal}.

%% Sum up allocator data in total blocks- and carriers size for all
%% allocators and per type of allocator.
%% Input  = Output from sort_allocator_types/3
%% Output = {[{"mseg_alloc",[{"segments_size",Value}]},
%%            {Type,[{blocks_size,Value},
%%                   {carriers_size,Value},
%%                   {mseg_carriers_size,Value}]},
%%            ...],
%%           {TotalBlocksSize,TotalCarriersSize}}
sum_allocator_data(AllocData,false) ->
    sum_allocator_data(AllocData,[],false);
sum_allocator_data(AllocData,true) ->
    sum_allocator_data(AllocData,[],{0,0}).

sum_allocator_data([{_Type,[]}|AllocData],TypeAcc,Total) ->
    sum_allocator_data(AllocData,TypeAcc,Total);
sum_allocator_data([{Type,Data}|AllocData],TypeAcc,Total) ->
    {TypeSum,NewTotal} = sum_type_data(Data,[],Total),
    sum_allocator_data(AllocData,[{Type,TypeSum}|TypeAcc],NewTotal);
sum_allocator_data([],TypeAcc,Total) ->
    {TypeAcc,Total}.

sum_type_data([{Key,Value}|Data],TypeAcc,Total) ->
    NewTotal =
	case Total of
	    false ->
		false;
	    {TotalBS,TotalCS} ->
		case lists:member(Key,?total_blocks_size) of
		    true ->
			{TotalBS+Value,TotalCS};
		    false ->
			case lists:member(Key,?total_carriers_size) of
			    true ->
				{TotalBS,TotalCS+Value};
			    false ->
				{TotalBS,TotalCS}
			end
		end
	end,
    NewTypeAcc =
	case lists:member(Key,?type_blocks_size) of
	    true ->
		update_value(blocks_size,Value,TypeAcc);
	    false ->
		case lists:member(Key,?type_carriers_size) of
		    true ->
			update_value(carriers_size,Value,TypeAcc);
		    false ->
			case lists:member(Key,?type_mseg_carriers_size) of
			    true ->
				update_value(mseg_carriers_size,Value,TypeAcc);
			    false ->
				%% "segments_size" for "mseg_alloc"
				update_value(Key,Value,TypeAcc)
			end
		end
	end,
    sum_type_data(Data,NewTypeAcc,NewTotal);
sum_type_data([],TypeAcc,Total) ->
    {TypeAcc,Total}.

update_value(Key,Value,Acc) ->
    case lists:keytake(Key,1,Acc) of
	false ->
	    [{Key,Value}|Acc];
	{value,{Key,Old},Acc1} ->
	    [{Key,Old+Value}|Acc1]
    end.

%%-----------------------------------------------------------------
%% Page with hash table information
hash_tables(File) ->
    case lookup_index(?hash_table) of
	[] ->
	    [];
	AllHashTables ->
	    Fd = open(File),
	    R = lists:map(fun({Name,Start}) ->
				  get_hashtableinfo(Fd,Name,Start) 
			  end, 
			  AllHashTables),
	    close(Fd),
	    R
    end.

get_hashtableinfo(Fd,Name,Start) ->
    pos_bof(Fd,Start),
    get_hashtableinfo1(Fd,#hash_table{name=Name}).

get_hashtableinfo1(Fd,HashTable) ->
    case line_head(Fd) of
	"size" ->
	    get_hashtableinfo1(Fd,HashTable#hash_table{size=val(Fd)});
	"used" ->
	    get_hashtableinfo1(Fd,HashTable#hash_table{used=val(Fd)});
	"objs" ->
	    get_hashtableinfo1(Fd,HashTable#hash_table{objs=val(Fd)});
	"depth" ->
	    get_hashtableinfo1(Fd,HashTable#hash_table{depth=val(Fd)});
    	"=" ++ _next_tag ->
	    HashTable;
	Other ->
	    unexpected(Fd,Other,"hash table information"),
	    HashTable
    end.

%%-----------------------------------------------------------------
%% Page with index table information
index_tables(File) ->
    case lookup_index(?index_table) of
	[] ->
	    [];
	AllIndexTables ->
	    Fd = open(File),
	    R = lists:map(fun({Name,Start}) ->
				  get_indextableinfo(Fd,Name,Start) 
			  end, 
			  AllIndexTables),
	    close(Fd),
	    R
    end.

get_indextableinfo(Fd,Name,Start) ->
    pos_bof(Fd,Start),
    get_indextableinfo1(Fd,#index_table{name=Name}).

get_indextableinfo1(Fd,IndexTable) ->
    case line_head(Fd) of
	"size" ->
	    get_indextableinfo1(Fd,IndexTable#index_table{size=val(Fd)});
	"used" ->
	    get_indextableinfo1(Fd,IndexTable#index_table{used=val(Fd)});
	"limit" ->
	    get_indextableinfo1(Fd,IndexTable#index_table{limit=val(Fd)});
	"rate" ->
	    get_indextableinfo1(Fd,IndexTable#index_table{rate=val(Fd)});
	"entries" ->
	    get_indextableinfo1(Fd,IndexTable#index_table{entries=val(Fd)});
    	"=" ++ _next_tag ->
	    IndexTable;
	Other ->
	    unexpected(Fd,Other,"index table information"),
	    IndexTable
    end.

    



%%-----------------------------------------------------------------
%% Expand a set of data which was shown in a truncated form on
get_expanded(File,Pos,Size) ->
    Fd = open(File),
    R = case file:pread(Fd,Pos,Size) of
	    {ok,Bin}->
		binary_to_list(Bin);
	    eof ->
		?space
	end,
    close(Fd),
    R.


replace_all(From,To,[From|Rest],Acc) ->
    replace_all(From,To,Rest,[To|Acc]);
replace_all(From,To,[Char|Rest],Acc) ->
    replace_all(From,To,Rest,[Char|Acc]);
replace_all(_From,_To,[],Acc) ->
    lists:reverse(Acc).


%%%-----------------------------------------------------------------
%%% Parse memory in crashdump version 0.1 and newer
%%%
parse_heap_term([$l|Line0], Addr, D0) ->	%Cons cell.
    {H,"|"++Line1,D1} = parse_term(Line0, D0),
    {T,Line,D2} = parse_term(Line1, D1),
    Term = [H|T],
    D = gb_trees:insert(Addr, Term, D2),
    {Term,Line,D};
parse_heap_term([$t|Line0], Addr, D) ->		%Tuple
    {N,":"++Line} = get_hex(Line0),
    parse_tuple(N, Line, Addr, D, []);
parse_heap_term([$F|Line0], Addr, D0) ->	%Float
    {N,":"++Line1} = get_hex(Line0),
    {Chars,Line} = get_chars(N, Line1),
    Term = list_to_float(Chars),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("B16#"++Line0, Addr, D0) ->	%Positive big number.
    {Term,Line} = get_hex(Line0),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("B-16#"++Line0, Addr, D0) ->	%Negative big number
    {Term0,Line} = get_hex(Line0),
    Term = -Term0,
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("B"++Line0, Addr, D0) ->	%Decimal big num (new in R10B-something).
    case string:to_integer(Line0) of
	{Int,Line} when is_integer(Int) ->
	    D = gb_trees:insert(Addr, Int, D0),
	    {Int,Line,D}
    end;
parse_heap_term([$P|Line0], Addr, D0) ->	% External Pid.
    {Pid0,Line} = get_id(Line0),
    Pid = "#CDVPid"++Pid0,
    D = gb_trees:insert(Addr, Pid, D0),
    {Pid,Line,D};
parse_heap_term([$p|Line0], Addr, D0) ->        % External Port.
    {Port0,Line} = get_id(Line0),
    Port = "#CDVPort"++Port0,
    D = gb_trees:insert(Addr, Port, D0),
    {Port,Line,D};
parse_heap_term("E"++Line0, Addr, D0) ->	%Term encoded in external format.
    {Bin,Line} = get_binary(Line0),
    Term = binary_to_term(Bin),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("Yh"++Line0, Addr, D0) ->	%Heap binary.
    {Term,Line} = get_binary(Line0),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("Yc"++Line0, Addr, D0) ->	%Reference-counted binary.
    {Binp,":"++Line1} = get_hex(Line0),
    {First,":"++Line2} = get_hex(Line1),
    {Sz,Line} = get_hex(Line2),
    Term = case gb_trees:lookup(Binp, D0) of
	       {value,<<_:First/binary,T:Sz/binary,_/binary>>} -> T;
	       {value,{'#CDVTooBig',binary,Pos}} -> cdvbin(Sz,Pos);
	       {value,'#CDVTruncatedBinary'} -> '#CDVTruncatedBinary';
	       none -> '#CDVNonexistingBinary'
	   end,
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("Ys"++Line0, Addr, D0) ->	%Sub binary.
    {Binp,":"++Line1} = get_hex(Line0),
    {First,":"++Line2} = get_hex(Line1),
    {Sz,Line} = get_hex(Line2),
    Term = case gb_trees:lookup(Binp, D0) of
	       {value,<<_:First/binary,T:Sz/binary,_/binary>>} -> T;
	       {value,{'#CDVTooBig',binary,Pos}} -> cdvbin(Sz,Pos);
	       {value,'#CDVTruncatedBinary'} -> '#CDVTruncatedBinary';
	       none -> '#CDVNonexistingBinary'
	   end,
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D}.


parse_tuple(0, Line, Addr, D0, Acc) ->
    Tuple = list_to_tuple(lists:reverse(Acc)),
    D = gb_trees:insert(Addr, Tuple, D0),
    {Tuple,Line,D};
parse_tuple(N, Line0, Addr, D0, Acc) ->
    case parse_term(Line0, D0) of
	{Term,[$,|Line],D} when N > 1 ->
	    parse_tuple(N-1, Line, Addr, D, [Term|Acc]);
	{Term,Line,D}->
	    parse_tuple(N-1, Line, Addr, D, [Term|Acc])
    end.

parse_term([$H|Line0], D) ->			%Pointer to heap term.
    {Ptr,Line} = get_hex(Line0),
    deref_ptr(Ptr, Line, D);
parse_term([$N|Line], D) ->			%[] (nil).
    {[],Line,D};
parse_term([$I|Line0], D) ->			%Small.
    {Int,Line} = string:to_integer(Line0),
    {Int,Line,D};
parse_term([$A|_]=Line, D) ->			%Atom.
    parse_atom(Line, D);
parse_term([$P|Line0], D) ->			%Pid.
    {Pid,Line} = get_id(Line0),
    {"#CDVPid"++Pid,Line,D};
parse_term([$p|Line0], D) ->			%Port.
    {Port,Line} = get_id(Line0),
    {"#CDVPort"++Port,Line,D};
parse_term([$S|Str0], D) ->			%Information string.
    Str = lists:reverse(skip_blanks(lists:reverse(Str0))),
    {Str,[],D};
parse_term([$D|Line0], D) ->                    %DistExternal
    try
	{AttabSize,":"++Line1} = get_hex(Line0),
	{Attab, "E"++Line2} = parse_atom_translation_table(AttabSize, Line1, []),
	{Bin,Line3} = get_binary(Line2),
	{try
	     erts_debug:dist_ext_to_term(Attab, Bin)
	 catch
	     error:_ -> '<invalid-distribution-message>'
	 end,
	 Line3,
	 D}
    catch
	error:_ -> 
	    {'#CDVBadDistExt', skip_dist_ext(Line0), D}
    end.

skip_dist_ext(Line) ->
    skip_dist_ext(lists:reverse(Line), []).

skip_dist_ext([], SeqTraceToken) ->
    SeqTraceToken;
skip_dist_ext([$:| _], SeqTraceToken) ->
    [$:|SeqTraceToken];
skip_dist_ext([C|Cs], KeptCs) ->
    skip_dist_ext(Cs, [C|KeptCs]).

parse_atom([$A|Line0], D) ->
    {N,":"++Line1} = get_hex(Line0),
    {Chars, Line} = get_chars(N, Line1),
    {list_to_atom(Chars), Line, D}.

parse_atom_translation_table(0, Line0, As) ->
    {list_to_tuple(lists:reverse(As)), Line0};
parse_atom_translation_table(N, Line0, As) ->
    {A, Line1, _} = parse_atom(Line0, []),
    parse_atom_translation_table(N-1, Line1, [A|As]).
    
    

deref_ptr(Ptr, Line, D0) ->
    case gb_trees:lookup(Ptr, D0) of
	{value,Term} ->
	    {Term,Line,D0};
	none ->
	    case get(fd) of
		end_of_heap ->
		    {['#CDVIncompleteHeap'],Line,D0};
		Fd ->
		    case val(Fd) of
			"="++_ ->
			    put(fd, end_of_heap),
			    deref_ptr(Ptr, Line, D0);
			L ->
			    D = parse(L, D0),
			    deref_ptr(Ptr, Line, D)
		    end
	    end
    end.

get_hex(L) ->
    get_hex_1(L, 0).

get_hex_1([H|T]=L, Acc) ->
    case get_hex_digit(H) of
	none -> {Acc,L};
	Digit -> get_hex_1(T, (Acc bsl 4) bor Digit)
    end;
get_hex_1([], Acc) -> {Acc,[]}.

get_hex_digit(C) when $0 =< C, C =< $9 -> C-$0;
get_hex_digit(C) when $a =< C, C =< $f -> C-$a+10;
get_hex_digit(C) when $A =< C, C =< $F -> C-$A+10;
get_hex_digit(_) -> none.

skip_blanks([$\s|T]) ->
    skip_blanks(T);
skip_blanks([$\r|T]) ->
    skip_blanks(T);
skip_blanks([$\n|T]) ->
    skip_blanks(T);
skip_blanks([$\t|T]) ->
    skip_blanks(T);
skip_blanks(T) -> T.

get_chars(N, Line) ->
    get_chars(N, Line, []).

get_chars(0, Line, Acc) ->
    {lists:reverse(Acc),Line};
get_chars(N, [H|T], Acc) ->
    get_chars(N-1, T, [H|Acc]).

get_id(Line) ->
    get_id(Line, []).

get_id([$>|Line], Acc) ->
    {lists:reverse(Acc, [$>]),Line};
get_id([H|T], Acc) ->
    get_id(T, [H|Acc]).

get_label(L) ->
    get_label(L, []).

get_label([$:|Line], Acc) ->
    Label = lists:reverse(Acc),
    case get_hex(Label) of
	{Int,[]} ->
	    {Int,Line};
	_ ->
	    {list_to_atom(Label),Line}
    end;
get_label([H|T], Acc) ->
    get_label(T, [H|Acc]).

get_binary(Line0) ->
    {N,":"++Line} = get_hex(Line0),
    get_binary(N, Line, []).

get_binary(0, Line, Acc) ->
    {list_to_binary(lists:reverse(Acc)),Line};
get_binary(N, [A,B|Line], Acc) ->
    Byte = (get_hex_digit(A) bsl 4) bor get_hex_digit(B),
    get_binary(N-1, Line, [Byte|Acc]);
get_binary(_N, [], _Acc) ->
    {'#CDVTruncatedBinary',[]}.

cdvbin(Sz,Pos) ->
    "#CDVBin<"++integer_to_list(Sz)++","++integer_to_list(Pos)++">".


%%-----------------------------------------------------------------
%% Functions for accessing the cdv_dump_index_table
reset_index_table() ->
    ets:delete_all_objects(cdv_dump_index_table).

insert_index(Tag,Id,Pos) ->
    ets:insert(cdv_dump_index_table,{{Tag,Pos},Id}).

lookup_index(Tag) ->
    lookup_index(Tag,'$2').
lookup_index(Tag,Id) ->
    ets:select(cdv_dump_index_table,[{{{Tag,'$1'},Id},[],[{{Id,'$1'}}]}]).

lookup_index_chunk({'#CDVFirstChunk',Tag,Id}) ->
    ets:select(cdv_dump_index_table,
	       [{{{Tag,'$1'},Id},[],[{{Id,'$1'}}]}],
	       ?items_chunk_size);
lookup_index_chunk(Cont) ->
    ets:select(Cont).

%% Create a tag which can be used instead of an ets Continuation for
%% the first call to lookup_index_chunk.
first_chunk_pointer({Tag,Id}) ->
    {'#CDVFirstChunk',Tag,Id};
first_chunk_pointer(Tag) ->
    first_chunk_pointer({Tag,'$2'}).

count_index(Tag) ->
    ets:select_count(cdv_dump_index_table,[{{{Tag,'_'},'_'},[],[true]}]).
count_index(Tag,Id) ->
    ets:select_count(cdv_dump_index_table,[{{{Tag,'_'},Id},[],[true]}]).


%%-----------------------------------------------------------------
%% Convert tags read from crashdump to atoms used as first part of key
%% in cdv_dump_index_table
tag_to_atom("allocated_areas") -> ?allocated_areas;
tag_to_atom("allocator") -> ?allocator;
tag_to_atom("atoms") -> ?atoms;
tag_to_atom("binary") -> ?binary;
tag_to_atom("debug_proc_dictionary") -> ?debug_proc_dictionary;
tag_to_atom("end") -> ?ende;
tag_to_atom("erl_crash_dump") -> ?erl_crash_dump;
tag_to_atom("ets") -> ?ets;
tag_to_atom("fun") -> ?fu;
tag_to_atom("hash_table") -> ?hash_table;
tag_to_atom("hidden_node") -> ?hidden_node;
tag_to_atom("index_table") -> ?index_table;
tag_to_atom("instr_data") -> ?instr_data;
tag_to_atom("internal_ets") -> ?internal_ets;
tag_to_atom("loaded_modules") -> ?loaded_modules;
tag_to_atom("memory") -> ?memory;
tag_to_atom("mod") -> ?mod;
tag_to_atom("no_distribution") -> ?no_distribution;
tag_to_atom("node") -> ?node;
tag_to_atom("not_connected") -> ?not_connected;
tag_to_atom("num_atoms") -> ?num_atoms;
tag_to_atom("old_instr_data") -> ?old_instr_data;
tag_to_atom("port") -> ?port;
tag_to_atom("proc") -> ?proc;
tag_to_atom("proc_dictionary") -> ?proc_dictionary;
tag_to_atom("proc_heap") -> ?proc_heap;
tag_to_atom("proc_messages") -> ?proc_messages;
tag_to_atom("proc_stack") -> ?proc_stack;
tag_to_atom("timer") -> ?timer;
tag_to_atom("visible_node") -> ?visible_node;
tag_to_atom(UnknownTag) ->
    io:format("WARNING: Found unexpected tag:~s~n",[UnknownTag]),
    list_to_atom(UnknownTag).

%%%-----------------------------------------------------------------
%%% Create a page by sending chunk by chunk to crashdump_viewer_html
chunk_page(SessionId,File,TW,What,HtmlCB,HtmlExtra,ParseFun) ->
    Fd = open(File),
    case lookup_and_parse_index_chunk(first_chunk_pointer(What),Fd,ParseFun) of
	done ->
	    crashdump_viewer_html:chunk_page(HtmlCB,SessionId,TW,HtmlExtra,done);
	{Chunk,Cont} ->
	    HtmlInfo = crashdump_viewer_html:chunk_page(
			     HtmlCB,
			     SessionId,TW,HtmlExtra,Chunk),
	    chunk_page_1(Fd,HtmlInfo,SessionId,ParseFun,
			 lookup_and_parse_index_chunk(Cont,Fd,ParseFun))
    end.

chunk_page_1(_Fd,HtmlInfo,SessionId,_ParseFun,done) ->
    crashdump_viewer_html:chunk(SessionId,done,HtmlInfo);
chunk_page_1(Fd,HtmlInfo,SessionId,ParseFun,{Chunk,Cont}) ->
    crashdump_viewer_html:chunk(SessionId,Chunk,HtmlInfo),
    chunk_page_1(Fd,HtmlInfo,SessionId,ParseFun,
		 lookup_and_parse_index_chunk(Cont,Fd,ParseFun)).

lookup_and_parse_index_chunk(Pointer,Fd,ParseFun) ->
    case lookup_index_chunk(Pointer) of
	'$end_of_table' ->
	    close(Fd),
	    done;
	{Chunk,Cont} ->
	    R = lists:map(fun({Id,Start}) ->
				  pos_bof(Fd,Start),
				  ParseFun(Fd,Id)
			  end,
			  Chunk),
	    {R,Cont}
    end.
