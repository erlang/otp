%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
%% called from HTML pages via erl_scheme.
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
%% The table entry for a tag includes the start and end positions for
%% this item-information. All tags start with a "=" at the beginning of
%% a line.
%%
%% Process state
%% -------------
%% file: The name of the crashdump currently viewed.
%% procs_summary: Process summary represented by a list of 
%% #proc records. This is used for efficiency reasons when sorting
%% the process summary table instead of reading all processes from
%% the dump again.
%% sorted: atom(), indicated what item was last sorted in process summary.
%% This is needed so reverse sorting can be done.
%% shared_heap: 'true' if crashdump comes from a system running shared heap,
%% else 'false'.
%% wordsize: 4 | 8, the number of bytes in a word.
%% binaries: a gb_tree containing binaries or links to binaries in the dump
%%

%% User API
-export([start/0,stop/0]).

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
	 processes/2,
	 proc_details/2,
	 ports/2,
	 ets_tables/2,
	 timers/2,
	 fun_table/2,
	 atoms/2,
	 dist_info/2,
	 loaded_modules/2,
	 loaded_mod_details/2,
	 memory/2,
	 allocated_areas/2,
	 allocator_info/2,
	 hash_tables/2,
	 index_tables/2,
	 sort_procs/2,
	 expand/2,
	 expand_binary/2,
	 expand_memory/2,
	 next/2]).


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

-define(initial_proc_record(Pid),
	#proc{pid=Pid,
	      %% msg_q_len, reds and stack_heap are integers because it must 
	      %% be possible to sort on them. All other fields are strings
	      msg_q_len=0,reds=0,stack_heap=0,
	      %% for old dumps start_time, parent and number of heap frament
	      %% does not exist
	      start_time="unknown",
	      parent="unknown",
	      num_heap_frag="unknown", 
	      %% current_func can be both "current function" and
	      %% "last scheduled in for"
	      current_func={"Current Function",?space},
	      %% stack_dump, message queue and dictionaries should only be 
	      %% displayed as a link to "Expand" (if dump is from OTP R9B 
	      %% or newer)
	      _=?space}).

-record(state,{file,procs_summary,sorted,shared_heap=false,
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
processes(_Env,_Input) ->
    call(procs_summary).
ports(_Env,Input) -> % this is also called when a link to a port is clicked
    call({ports,Input}).
ets_tables(_Env,Input) ->
    call({ets_tables,Input}).
timers(_Env,Input) ->
    call({timers,Input}).
fun_table(_Env,_Input) ->
    call(funs).
atoms(_Env,_Input) ->
    call(atoms).
dist_info(_Env,_Input) ->
    call(dist_info).
loaded_modules(_Env,_Input) ->
    call(loaded_mods).
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
sort_procs(_Env,Input) ->
    call({sort_procs,Input}).

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
%%% Called when the "Next" link under atoms is clicked.
next(_Env,Input) ->
    call({next,Input}).

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
    ets:new(cdv_dump_index_table,[bag,named_table,public]),
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
    {ok,File0} = get_value("path",httpd:parse_query(Input)),
    File = 
	case File0 of
	    [$"|FileAndSome] ->
		%% Opera adds \"\" around the filename!
		[$"|Elif] = lists:reverse(FileAndSome),
		lists:reverse(Elif);
	    _ ->
		File0
	end,
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
    NumAtoms = GenInfo#general_info.num_atoms,
    {WS,SH} = parse_vsn_str(GenInfo#general_info.system_vsn,4,false),
    Reply = crashdump_viewer_html:general_info(GenInfo),
    {reply,Reply,State#state{shared_heap=SH,wordsize=WS,num_atoms=NumAtoms}};
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
	case truncated_warning([{"=proc",Pid}]) of
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
handle_call({next,Input},_From,State=#state{file=File}) ->
    [{"pos",Pos},{"num",N},{"start",Start},{"what",What}] =
	httpd:parse_query(Input),
    Tags = related_tags(What),
    TW = truncated_warning(Tags),
    Next = get_next(File,list_to_integer(Pos),list_to_integer(N),
		    list_to_integer(Start),What),
    Reply = crashdump_viewer_html:next(Next,TW),
    {reply,Reply,State};
handle_call(general_info,_From,State=#state{file=File}) ->
    GenInfo=general_info(File),
    Reply = crashdump_viewer_html:general_info(GenInfo),
    {reply,Reply,State};
handle_call(procs_summary,_From,State=#state{file=File,shared_heap=SH}) ->
    ProcsSummary =
	case State#state.procs_summary of
	    undefined -> procs_summary(File);
	    PS -> PS
	end,
    TW = truncated_warning(["=proc"]),
    Reply = crashdump_viewer_html:procs_summary("pid",ProcsSummary,TW,SH),
    {reply,Reply,State#state{procs_summary=ProcsSummary,sorted="pid"}};
handle_call({sort_procs,Input}, _From, State=#state{shared_heap=SH}) ->
    {ok,Sort} = get_value("sort",httpd:parse_query(Input)),
    {ProcsSummary,Sorted} = do_sort_procs(Sort,
					  State#state.procs_summary,
					  State#state.sorted),
    TW = truncated_warning(["=proc"]),
    Reply = crashdump_viewer_html:procs_summary(Sort,ProcsSummary,TW,SH),
    {reply,Reply,State#state{sorted=Sorted}};
handle_call({proc_details,Input},_From,State=#state{file=File,shared_heap=SH}) ->
    {ok,Pid} = get_value("pid",httpd:parse_query(Input)),
    Reply = 
	case get_proc_details(File,Pid) of
	    {ok,Proc} -> 
		TW = truncated_warning([{"=proc",Pid}]),
		crashdump_viewer_html:proc_details(Pid,Proc,TW,SH);
	    {other_node,Node} -> 
		TW = truncated_warning(["=visible_node",
					"=hidden_node",
					"=not_connected"]),
		crashdump_viewer_html:nods(Node,TW);
	    not_found -> 
		crashdump_viewer_html:info_page(["Could not find process: ",
						 Pid],?space)
	end,
    {reply, Reply, State};
handle_call({ports,Input},_From,State=#state{file=File}) ->
    Reply = 
	case get_value("port",httpd:parse_query(Input)) of
	    {ok,P} -> 
		Id = [$#|P],
		case get_port(File,Id) of
		    {ok,PortInfo} ->
			TW = truncated_warning([{"=port",Id}]),
			crashdump_viewer_html:ports(Id,[PortInfo],TW);
		    {other_node,Node} ->
			TW = truncated_warning(["=visible_node",
						"=hidden_node",
						"=not_connected"]),
			crashdump_viewer_html:nods(Node,TW);
		    not_found -> 
			crashdump_viewer_html:info_page(
			  ["Could not find port: ",Id],?space)
		end;
	    error -> % no port identity in Input - get all ports
		Ports=get_ports(File),
		TW = truncated_warning(["=port"]),
		crashdump_viewer_html:ports("Port Information",Ports,TW)
	end,
    {reply,Reply,State};
handle_call({ets_tables,Input},_From,State=#state{file=File,wordsize=WS}) ->
    {Pid,Heading,InternalEts} = 
	case get_value("pid",httpd:parse_query(Input)) of
	    {ok,P} -> 
		{P,["ETS Tables for Process ",P],[]};
	    error -> 
		I = get_internal_ets_tables(File,WS),
		{'_',"ETS Table Information",I}
	end,
    EtsTables = get_ets_tables(File,Pid,WS),
    TW = truncated_warning(["=ets"]),
    Reply = crashdump_viewer_html:ets_tables(Heading,EtsTables,InternalEts,TW),
    {reply,Reply,State};
handle_call({timers,Input},_From,State=#state{file=File}) ->
    {Pid,Heading} = 
	case get_value("pid",httpd:parse_query(Input)) of
	    {ok,P} -> {P,["Timers for Process ",P]};
	    error -> {'_',"Timer Information"}
	end,
    Timers=get_timers(File,Pid),
    TW = truncated_warning(["=timer"]),
    Reply = crashdump_viewer_html:timers(Heading,Timers,TW),
    {reply,Reply,State};
handle_call(dist_info,_From,State=#state{file=File}) ->
    Nods=nods(File),
    TW = truncated_warning(["=visible_node","=hidden_node","=not_connected"]),
    Reply = crashdump_viewer_html:nods(Nods,TW),
    {reply,Reply,State};
handle_call(loaded_mods,_From,State=#state{file=File}) ->
    LoadedMods=loaded_mods(File),
    TW = truncated_warning(["=mod"]),
    Reply = crashdump_viewer_html:loaded_mods(LoadedMods,TW),
    {reply,Reply,State};
handle_call({loaded_mod_details,Input},_From,State=#state{file=File}) ->
    {ok,Mod} = get_value("mod",httpd:parse_query(Input)),
    ModInfo = get_loaded_mod_details(File,Mod),
    TW = truncated_warning([{"=mod",Mod}]),
    Reply = crashdump_viewer_html:loaded_mod_details(ModInfo,TW),
    {reply,Reply,State};
handle_call(funs,_From,State=#state{file=File}) ->
    Funs=funs(File),
    TW = truncated_warning(["=fun"]),
    Reply = crashdump_viewer_html:funs(Funs,TW),
    {reply,Reply,State};
handle_call(atoms,_From,State=#state{file=File,num_atoms=Num}) ->
    Atoms=atoms(File),
    TW = truncated_warning(["=atoms","=num_atoms"]),
    Reply = crashdump_viewer_html:atoms(Atoms,Num,TW),
    {reply,Reply,State};
handle_call(memory,_From,State=#state{file=File}) ->
    Memory=memory(File),
    TW = truncated_warning(["=memory"]),
    Reply = crashdump_viewer_html:memory(Memory,TW),
    {reply,Reply,State};
handle_call(allocated_areas,_From,State=#state{file=File}) ->
    AllocatedAreas=allocated_areas(File),
    TW = truncated_warning(["=allocated_areas"]),
    Reply = crashdump_viewer_html:allocated_areas(AllocatedAreas,TW),
    {reply,Reply,State};
handle_call(allocator_info,_From,State=#state{file=File}) ->
    SlAlloc=allocator_info(File),
    TW = truncated_warning(["=allocator"]),
    Reply = crashdump_viewer_html:allocator_info(SlAlloc,TW),
    {reply,Reply,State};
handle_call(hash_tables,_From,State=#state{file=File}) ->
    HashTables=hash_tables(File),
    TW = truncated_warning(["=hash_table","=index_table"]),
    Reply = crashdump_viewer_html:hash_tables(HashTables,TW),
    {reply,Reply,State};
handle_call(index_tables,_From,State=#state{file=File}) ->
    IndexTables=index_tables(File),
    TW = truncated_warning(["=hash_table","=index_table"]),
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
%% Eg if this is {"=proc","<0.30.0>"}, we should warn if the dump was
%% truncated in {"=proc","<0.29.0>"} or earlier
truncated_earlier({"=proc",Pid}) ->
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
pos_bof(Fd,Pos) ->
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
       menu_item(0, {"./ets_tables","ETS tables"}, 0),
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
			"=erl_crash_dump" ->
			    ets:delete_all_objects(cdv_dump_index_table),
			    ets:insert(cdv_dump_index_table,{Tag,Id,N1+1}),
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

indexify(Fd,<<"\n=",TagAndRest/binary>>,N) ->
    {Tag,Id,Rest,N1} = tag(Fd,TagAndRest,N+2),
    ets:insert(cdv_dump_index_table,{Tag,Id,N1+1}), % +1 to get past newline
    put(last_tag,{Tag,Id}),
    indexify(Fd,Rest,N1);
indexify(Fd,<<>>,N) ->
    case read(Fd) of
	{ok,Chunk} when is_binary(Chunk) ->
	    indexify(Fd,Chunk,N);
        eof ->
	    eof
    end;
indexify(Fd,<<$\n>>,N) ->
    %% This clause is needed in case the chunk ends with a newline and
    %% the next chunk starts with a tag (i.e. "\n=....")
    case read(Fd) of
	{ok,Chunk} when is_binary(Chunk) ->
	    indexify(Fd,<<$\n,Chunk/binary>>,N);
        eof ->
	    eof
    end;
indexify(Fd,<<_Char:8,Rest/binary>>,N) ->
    indexify(Fd,Rest,N+1).

tag(Fd,Bin,N) ->
    tag(Fd,Bin,N,[],[],tag).
tag(_Fd,<<$\n:8,_/binary>>=Rest,N,Gat,Di,_Now) ->
    {[$=|lists:reverse(Gat)],lists:reverse(Di),Rest,N};
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
	    {[$=|lists:reverse(Gat)],lists:reverse(Di),<<>>,N}
    end.

check_if_truncated() ->
    case get(last_tag) of
	{"=end",_} ->
	    put(truncated,false),
	    put(truncated_proc,false);
	TruncatedTag ->
	    put(truncated,true),
	    find_truncated_proc(TruncatedTag)
    end.
	    
find_truncated_proc({"=atom",_Id}) ->
    put(truncated_proc,false);
find_truncated_proc({Tag,Pid}) ->
    case is_proc_tag(Tag) of
	true -> 
	    put(truncated_proc,Pid);
	false -> 
	    %% This means that the dump is truncated between "=proc" and
	    %% "=proc_heap" => memory info is missing for all procs.
	    put(truncated_proc,"<0.0.0>")
    end.

is_proc_tag(Tag)  when Tag=="=proc";
		       Tag=="=proc_dictionary";
		       Tag=="=proc_messages";
		       Tag=="=proc_dictionary";
		       Tag=="=debug_proc_dictionary";
		       Tag=="=proc_stack";
		       Tag=="=proc_heap" ->
    true;
is_proc_tag(_) ->
    false.

related_tags("Atoms") ->
    ["=atoms","=num_atoms"].

%%% Inform the crashdump_viewer_server that a background job is completed.
background_done(Result) ->
    Dict = get(),
    cast({background_done,Result,Dict}).    

background_status(Status) ->
    cast({background_status,Status}).

%%%-----------------------------------------------------------------
%%% Functions for reading information from the dump
general_info(File) ->
    [{"=erl_crash_dump",_Id,Start}] = 
	ets:lookup(cdv_dump_index_table,"=erl_crash_dump"),
    Fd = open(File),
    pos_bof(Fd,Start),
    Created = case get_rest_of_line(Fd) of
		  {eof,SomeOfLine} -> SomeOfLine;
		  WholeLine -> WholeLine
	      end,

    GI0 = get_general_info(Fd,#general_info{created=Created,_=?space}),
    GI = case GI0#general_info.num_atoms of
	    ?space -> GI0#general_info{num_atoms=get_num_atoms(Fd)};
	    _ -> GI0
	end,

    {MemTot,MemMax} = 
	case ets:lookup(cdv_dump_index_table,"=memory") of
	    [{"=memory",_,MemStart}] ->
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
    {NumProcs,NumEts,NumFuns} = count(),
    NodeName = 
	case ets:lookup(cdv_dump_index_table,"=node") of
	    [{"=node",N,_Start}] ->
		N;
	    [] ->
		case ets:lookup(cdv_dump_index_table,"=no_distribution") of
		    [_] -> "nonode@nohost";
		    [] -> "unknown"
		end
	end,

    InstrInfo =
	case ets:member(cdv_dump_index_table,"=old_instr_data") of
	    true ->
		old_instr_data;
	    false ->
		case ets:member(cdv_dump_index_table,"=instr_data") of
		    true ->
			instr_data;
		    false ->
			false
		end
	end,
    GI#general_info{node_name=NodeName,
		    num_procs=integer_to_list(NumProcs),
		    num_ets=integer_to_list(NumEts),
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
    case ets:match(cdv_dump_index_table,{"=hash_table","atom_tab",'$1'}) of
	[[Pos]] -> 
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
    case ets:lookup(cdv_dump_index_table,"=num_atoms") of
	[] -> 
	    ?space;
	[{"=num_atoms",NA,_Pos}] -> 
	    %% If dump is translated this will exist
	    case get(truncated) of
		true ->
		    [NA," (visible in dump)"]; % might be more
		false ->
		    NA
	    end
    end.

count() ->
    {ets:select_count(cdv_dump_index_table,count_ms("=proc")),
     ets:select_count(cdv_dump_index_table,count_ms("=ets")),
     ets:select_count(cdv_dump_index_table,count_ms("=fun"))}.

count_ms(Tag) ->
    [{{Tag,'_','_'},[],[true]}].


procs_summary(File) ->
    AllProcs = ets:lookup(cdv_dump_index_table,"=proc"),
    Fd = open(File),
    R = lists:map(fun({"=proc",Pid,Start}) -> 
			  pos_bof(Fd,Start),
			  get_procinfo(Fd,fun main_procinfo/4,
				       ?initial_proc_record(Pid))
		  end, 
		  AllProcs),
    close(Fd),
    R.

get_proc_details(File,Pid) ->
    DumpVsn = ets:lookup_element(cdv_dump_index_table,"=erl_crash_dump",2),
    case ets:match(cdv_dump_index_table,{"=proc",Pid,'$1'}) of
	[[Start]] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    Proc0 = 
		case DumpVsn of
		    "0.0" -> 
			%% Old version (translated)
			?initial_proc_record(Pid);
		    _ ->
			(?initial_proc_record(Pid))#proc{
			  stack_dump=if_exist("=proc_stack",Pid),
			  msg_q=if_exist("=proc_messages",Pid),
			  dict=if_exist("=proc_dictionary",Pid),
			  debug_dict=if_exist("=debug_proc_dictionary",Pid)}
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
    case ets:select_count(cdv_dump_index_table,[{{Tag,Key,'_'},[],[true]}]) of
	0 -> 
	    Tag1 = 
		case is_proc_tag(Tag) of
		    true -> "=proc";
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
	   fun({Tag,Id,Start}) when Tag=:="=visible_node", Id=:=Channel -> 
		   {"Visible Node",Start};
	      ({Tag,Id,Start}) when Tag=:="=hidden_node", Id=:=Channel ->
		   {"Hidden Node",Start};
	      ({Tag,Id,Start}) when Tag=:="=not_connected", Id=:=Channel -> 
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
	    "Dictionary" -> read_dictionary(Fd,"=proc_dictionary",Pid,Dict);
	    "DebugDictionary" -> read_dictionary(Fd,"=debug_proc_dictionary",Pid,Dict)
	end,
    erase(fd),
    close(Fd),
    Expanded.
    
%%%
%%% Read binaries.
%%%
read_binaries(Fd) ->
    AllBinaries = ets:match(cdv_dump_index_table,{"=binary",'$1','$2'}),
    read_binaries(Fd,AllBinaries, gb_trees:empty()).

read_binaries(Fd,[[Addr0,Pos]|Bins],Dict0) ->
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
    case ets:match(cdv_dump_index_table,{"=proc_stack",Pid,'$1'}) of
	[[Start]] ->
	    pos_bof(Fd,Start),
	    read_stack_dump1(Fd,Dict,[]);
	[] ->
	    []
    end.
read_stack_dump1(Fd,Dict,Acc) ->
    %% This function is never called if the dump is truncated in "=proc_heap:Pid"
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
    case ets:match(cdv_dump_index_table,{"=proc_messages",Pid,'$1'}) of
	[[Start]] ->
	    pos_bof(Fd,Start),
	    read_messages1(Fd,Dict,[]);
	[] ->
	    []
    end.
read_messages1(Fd,Dict,Acc) ->
    %% This function is never called if the dump is truncated in "=proc_heap:Pid"
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
    case ets:match(cdv_dump_index_table,{Tag,Pid,'$1'}) of
	[[Start]] ->
	    pos_bof(Fd,Start),
	    read_dictionary1(Fd,Dict,[]);
	[] ->
	    []
    end.
read_dictionary1(Fd,Dict,Acc) ->
    %% This function is never called if the dump is truncated in "=proc_heap:Pid"
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
    case ets:match(cdv_dump_index_table,{"=proc_heap",Pid,'$2'}) of
	[[Pos]] ->
	    pos_bof(Fd,Pos),
	    read_heap(Dict0);
	[] ->
	    Dict0
    end.

read_heap(Dict0) ->
    %% This function is never called if the dump is truncated in "=proc_heap:Pid"
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


do_sort_procs("state",Procs,"state") ->
    {lists:reverse(lists:keysort(#proc.state,Procs)),"rstate"};
do_sort_procs("state",Procs,_) ->
    {lists:keysort(#proc.state,Procs),"state"};
do_sort_procs("pid",Procs,"pid") ->
    {lists:reverse(Procs),"rpid"};
do_sort_procs("pid",Procs,_) ->
    {Procs,"pid"};
do_sort_procs("msg_q_len",Procs,"msg_q_len") ->
    {lists:keysort(#proc.msg_q_len,Procs),"rmsg_q_len"};
do_sort_procs("msg_q_len",Procs,_) ->
    {lists:reverse(lists:keysort(#proc.msg_q_len,Procs)),"msg_q_len"};
do_sort_procs("reds",Procs,"reds") ->
    {lists:keysort(#proc.reds,Procs),"rreds"};
do_sort_procs("reds",Procs,_) ->
    {lists:reverse(lists:keysort(#proc.reds,Procs)),"reds"};
do_sort_procs("mem",Procs,"mem") ->
    {lists:keysort(#proc.stack_heap,Procs),"rmem"};
do_sort_procs("mem",Procs,_) ->
    {lists:reverse(lists:keysort(#proc.stack_heap,Procs)),"mem"};
do_sort_procs("init_func",Procs,"init_func") ->
    {lists:reverse(lists:keysort(#proc.init_func,Procs)),"rinit_func"};
do_sort_procs("init_func",Procs,_) ->
    {lists:keysort(#proc.init_func,Procs),"init_func"};
do_sort_procs("name_func",Procs,"name_func") ->
    {lists:reverse(lists:keysort(#proc.name,Procs)),"rname_func"};
do_sort_procs("name_func",Procs,_) ->
    {lists:keysort(#proc.name,Procs),"name_func"};
do_sort_procs("name",Procs,Sorted) ->
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
    

get_port(File,Port) ->
    case ets:match(cdv_dump_index_table,{"=port",Port,'$1'}) of
	[[Start]] ->
	    Fd = open(File),
	    R = get_portinfo(Fd,Port,Start),
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

get_ports(File) ->
    Ports = ets:lookup(cdv_dump_index_table,"=port"),
    Fd = open(File),
    R = lists:map(fun({"=port",Id,Start}) -> get_portinfo(Fd,Id,Start) end, 
		  Ports),
    close(Fd),
    R.


get_portinfo(Fd,Id,Start) ->
    pos_bof(Fd,Start),
    get_portinfo(Fd,#port{id=Id,_=?space}).

get_portinfo(Fd,Port) ->
    case line_head(Fd) of
	"Slot" ->
	    get_portinfo(Fd,Port#port{slot=val(Fd)});
	"Connected" ->
	    get_portinfo(Fd,Port#port{connected=val(Fd)});
	"Links" ->
	    get_portinfo(Fd,Port#port{links=val(Fd)});
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

get_ets_tables(File,Pid,WS) ->
    EtsTables = ets:match_object(cdv_dump_index_table,{"=ets",Pid,'_'}),
    Fd = open(File),
    R = lists:map(fun({"=ets",P,Start}) -> 
			  get_etsinfo(Fd,P,Start,WS) 
		  end, 
		  EtsTables),
    close(Fd),
    R.

get_internal_ets_tables(File,WS) ->
    InternalEts = ets:match_object(cdv_dump_index_table,
				   {"=internal_ets",'_','_'}),
    Fd = open(File),
    R = lists:map(fun({"=internal_ets",Descr,Start}) ->
			  {Descr,get_etsinfo(Fd,undefined,Start,WS)}
		  end,
		  InternalEts),
    close(Fd),
    R.
    
get_etsinfo(Fd,Pid,Start,WS) ->
    pos_bof(Fd,Start),
    get_etsinfo(Fd,#ets_table{pid=Pid,type="hash",_=?space},WS).

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

get_timers(File,Pid) ->
    Timers = ets:match_object(cdv_dump_index_table,{"=timer",Pid,'$1'}),
    Fd = open(File),
    R = lists:map(fun({"=timer",P,Start}) -> 
			  get_timerinfo(Fd,P,Start) 
		  end, 
		  Timers),
    close(Fd),
    R.

get_timerinfo(Fd,Pid,Start) ->
    pos_bof(Fd,Start),
    get_timerinfo(Fd,#timer{pid=Pid,_=?space}).

get_timerinfo(Fd,Timer) ->
    case line_head(Fd) of
	"Message" ->
	    get_timerinfo(Fd,Timer#timer{msg=val(Fd)});
	"Time left" ->
	    get_timerinfo(Fd,Timer#timer{time=val(Fd)});
	"=" ++ _next_tag ->
	    Timer;
	Other ->
	    unexpected(Fd,Other,"timer info"),
	    Timer
    end.

nods(File) ->
    case ets:lookup(cdv_dump_index_table,"=no_distribution") of
	[] ->
	    V = ets:lookup(cdv_dump_index_table,"=visible_node"),
	    H = ets:lookup(cdv_dump_index_table,"=hidden_node"),
	    N = ets:lookup(cdv_dump_index_table,"=not_connected"),
	    Fd = open(File),
	    Visible = lists:map(
			fun({"=visible_node",Channel,Start}) -> 
				get_nodeinfo(Fd,Channel,Start)
			end, 
			V),
	    Hidden = lists:map(
		       fun({"=hidden_node",Channel,Start}) -> 
			       get_nodeinfo(Fd,Channel,Start)
		       end, 
		       H),
	    NotConnected = lists:map(
			     fun({"=not_connected",Channel,Start}) -> 
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
    get_nodeinfo(Fd,#nod{channel=Channel,_=?space}).

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

loaded_mods(File) ->
    case ets:lookup(cdv_dump_index_table,"=loaded_modules") of
	[{"=loaded_modules",_,StartTotal}] ->
	    Fd = open(File),
	    pos_bof(Fd,StartTotal),
	    {CC,OC} = get_loaded_mod_totals(Fd,{"unknown","unknown"}),
	    
	    Mods = ets:lookup(cdv_dump_index_table,"=mod"),
	    LM = lists:map(fun({"=mod",M,Start}) -> 
				   pos_bof(Fd,Start),
				   InitLM = #loaded_mod{mod=M,_=?space},
				   get_loaded_mod_info(Fd,InitLM,
						       fun main_modinfo/3)
			   end, 
			   Mods),
	    close(Fd),
	    {CC,OC,LM};
	[] ->
	    {"unknown","unknown",[]}
    end.

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

get_loaded_mod_details(File,Mod) ->
    [[Start]] = ets:match(cdv_dump_index_table,{"=mod",Mod,'$1'}),
    Fd = open(File),
    pos_bof(Fd,Start),
    InitLM = #loaded_mod{mod=Mod,old_size="No old code exists",
			 _="No information available"},
    ModInfo = get_loaded_mod_info(Fd,InitLM,fun all_modinfo/3),
    close(Fd),
    ModInfo.

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
    


funs(File) ->
    case ets:lookup(cdv_dump_index_table,"=fun") of
	[] ->
	    [];
	AllFuns ->
	    Fd = open(File),
	    R = lists:map(fun({"=fun",_,Start}) -> 
				  get_funinfo(Fd,Start) 
			  end, 
			  AllFuns),
	    close(Fd),
	    R
    end.

get_funinfo(Fd,Start) ->
    pos_bof(Fd,Start),
    get_funinfo1(Fd,#fu{_=?space}).

get_funinfo1(Fd,Fu) ->
    case line_head(Fd) of
	"Module" ->
	    get_funinfo1(Fd,Fu#fu{module=val(Fd)});
	"Uniq" ->
	    get_funinfo1(Fd,Fu#fu{uniq=val(Fd)});
	"Index" ->
	    get_funinfo1(Fd,Fu#fu{index=val(Fd)});
	"Address" ->
	    get_funinfo1(Fd,Fu#fu{address=val(Fd)});
	"Native_address" ->
	    get_funinfo1(Fd,Fu#fu{native_address=val(Fd)});
	"Refc" ->
	    get_funinfo1(Fd,Fu#fu{refc=val(Fd)});
	"=" ++ _next_tag ->
	    Fu;
	Other ->
	    unexpected(Fd,Other,"fun info"),
	    Fu
    end.

atoms(File) ->
    case ets:lookup(cdv_dump_index_table,"=atoms") of
	[{_atoms,_Id,Start}] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    R = case get_n_lines_of_tag(Fd,100) of
		    {all,N,Lines} ->
			{n_lines,1,N,"Atoms",Lines};
		    {part,100,Lines} ->
			{n_lines,1,100,"Atoms",Lines,get(pos)};
		    empty ->
			[]
		end,
	    close(Fd),
	    R;
	_ ->
	    []
    end.

memory(File) ->
    case ets:lookup(cdv_dump_index_table,"=memory") of
	[{"=memory",_,Start}] ->
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
	    
allocated_areas(File) ->
    case ets:lookup(cdv_dump_index_table,"=allocated_areas") of
	[{"=allocated_areas",_,Start}] ->
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
	    
allocator_info(File) ->
    case ets:lookup(cdv_dump_index_table,"=allocator") of
	[] ->
	    [];
	AllAllocators ->
	    Fd = open(File),
	    R = lists:map(fun({"=allocator",Heading,Start}) -> 
				  {Heading,get_allocatorinfo(Fd,Start)} 
			  end, 
			  AllAllocators),
	    close(Fd),
	    R
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


hash_tables(File) ->
    case ets:lookup(cdv_dump_index_table,"=hash_table") of
	[] ->
	    [];
	AllHashTables ->
	    Fd = open(File),
	    R = lists:map(fun({"=hash_table",Name,Start}) -> 
				  get_hashtableinfo(Fd,Name,Start) 
			  end, 
			  AllHashTables),
	    close(Fd),
	    R
    end.

get_hashtableinfo(Fd,Name,Start) ->
    pos_bof(Fd,Start),
    get_hashtableinfo1(Fd,#hash_table{name=Name,_=?space}).

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

index_tables(File) ->
    case ets:lookup(cdv_dump_index_table,"=index_table") of
	[] ->
	    [];
	AllIndexTables ->
	    Fd = open(File),
	    R = lists:map(fun({"=index_table",Name,Start}) -> 
				  get_indextableinfo(Fd,Name,Start) 
			  end, 
			  AllIndexTables),
	    close(Fd),
	    R
    end.

get_indextableinfo(Fd,Name,Start) ->
    pos_bof(Fd,Start),
    get_indextableinfo1(Fd,#index_table{name=Name,_=?space}).

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
    	"=" ++ _next_tag ->
	    IndexTable;
	Other ->
	    unexpected(Fd,Other,"index table information"),
	    IndexTable
    end.

    



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


get_next(File,Pos,N0,Start,What) ->
    Fd = open(File),
    pos_bof(Fd,Pos),
    R = case get_n_lines_of_tag(Fd,N0) of
	    {all,N,Lines} ->
		{n_lines,Start,N,What,Lines};
	    {part,N,Lines} ->
		{n_lines,Start,N,What,Lines,get(pos)}
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
