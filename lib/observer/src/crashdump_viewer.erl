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
-module(crashdump_viewer).

%% 
%% This module is the main module in the crashdump viewer. It implements
%% the server backend for the crashdump viewer tool.
%% 
%% Tables
%% ------
%% cdv_dump_index_table: This table holds all tags read from the
%% crashdump.  Each tag indicates where the information about a
%% specific item starts.  The table entry for a tag includes the start
%% position for this item-information. In a crash dump file, all tags
%% start with a "=" at the beginning of a line.
%%
%% Process state
%% -------------
%% file: The name of the crashdump currently viewed.
%% dump_vsn: The version number of the crashdump
%% wordsize: 4 | 8, the number of bytes in a word.
%% binaries: a gb_tree containing binaries or links to binaries in the dump
%%

%% User API
-export([start/0,start/1,stop/0,script_start/0,script_start/1]).

%% GUI API
-export([start_link/0]).
-export([read_file/1,
	 general_info/0,
	 processes/0,
	 proc_details/1,
	 port/1,
	 ports/0,
	 ets_tables/1,
	 internal_ets_tables/0,
	 timers/1,
	 funs/0,
	 atoms/0,
	 dist_info/0,
	 node_info/1,
	 loaded_modules/0,
	 loaded_mod_details/1,
	 memory/0,
	 allocated_areas/0,
	 allocator_info/0,
	 hash_tables/0,
	 index_tables/0,
	 schedulers/0,
	 expand_binary/1]).

%% Library function
-export([to_proplist/2, to_value_list/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

%% Debug support
-export([debug/1,stop_debug/0]).

-include("crashdump_viewer.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER, crashdump_viewer_server).
-define(call_timeout,3600000).
-define(chunk_size,1000). % number of bytes read from crashdump at a time
-define(max_line_size,100). % max number of bytes (i.e. characters) the
			    % line_head/1 function can return
-define(not_available,"N/A").


%% All possible tags - use macros in order to avoid misspelling in the code
-define(allocated_areas,allocated_areas).
-define(allocator,allocator).
-define(atoms,atoms).
-define(binary,binary).
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
-define(old_instr_data,old_instr_data).
-define(port,port).
-define(proc,proc).
-define(proc_dictionary,proc_dictionary).
-define(proc_heap,proc_heap).
-define(proc_messages,proc_messages).
-define(proc_stack,proc_stack).
-define(scheduler,scheduler).
-define(timer,timer).
-define(visible_node,visible_node).


-record(state,{file,dump_vsn,wordsize=4,num_atoms="unknown",binaries}).

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
    start(undefined).
start(File) ->
    cdv_wx:start(File).

stop() ->
    case whereis(?SERVER) of
	undefined ->
	    ok;
	Pid ->
	    Ref = erlang:monitor(process,Pid),
	    cast(stop),
	    receive {'DOWN', Ref, process, Pid, _} -> ok end
    end.

%%%-----------------------------------------------------------------
%%% Start crashdump_viewer via the cdv script located in
%%% $OBSERVER_PRIV_DIR/bin
script_start() ->
    do_script_start(fun() -> start() end),
    erlang:halt().
script_start([FileAtom]) ->
    File = atom_to_list(FileAtom),
    case filelib:is_regular(File) of
	true ->
	    do_script_start(fun() -> start(File) end);
	false ->
	    io:format("cdv error: the given file does not exist\n"),
	    usage()
    end,
    erlang:halt();
script_start(_) ->
    usage(),
    erlang:halt().

do_script_start(StartFun) ->
    process_flag(trap_exit,true),
    case StartFun() of
	ok ->
	    case whereis(cdv_wx) of
		Pid when is_pid(Pid) ->
		    link(Pid),
		    receive
			{'EXIT', Pid, normal} ->
			    ok;
			{'EXIT', Pid, Reason} ->
			    io:format("\ncdv crash: ~p\n",[Reason])
		    end;
		_ ->
		    io:format("\ncdv crash: ~p\n",[unknown_reason])
	    end;
	Error ->
	    io:format("\ncdv start failed: ~p\n",[Error])
    end.

usage() ->
    io:format(
      "usage: cdv [file]\n"
      "\tThe \'file\' must be an existing erlang crash dump.\n"
      "\tIf omitted a file dialog will be opened.\n",
      []).

%%====================================================================
%% External functions
%%====================================================================
%%%--------------------------------------------------------------------
%%% Start the server - called by cdv_wx
start_link() ->
    case whereis(?SERVER) of
	undefined ->
	    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
	Pid ->
	    {ok,Pid}
    end.

%%%-----------------------------------------------------------------
%%% Called by cdv_wx
read_file(File) ->
    cast({read_file,File}).

%%%-----------------------------------------------------------------
%%% The following functions are called when the different tabs are
%%% created
general_info() ->
    call(general_info).
processes() ->
    call(procs_summary).
ports() ->
    call(ports).
ets_tables(Owner) ->
    call({ets_tables,Owner}).
internal_ets_tables() ->
    call(internal_ets_tables).
timers(Owner) ->
    call({timers,Owner}).
funs() ->
    call(funs).
atoms() ->
    call(atoms).
dist_info() ->
    call(dist_info).
node_info(Channel) ->
    call({node_info,Channel}).
loaded_modules() ->
    call(loaded_mods).
loaded_mod_details(Mod) ->
    call({loaded_mod_details,Mod}).
memory() ->
    call(memory).
allocated_areas() ->
    call(allocated_areas).
allocator_info() ->
    call(allocator_info).
hash_tables() ->
    call(hash_tables).
index_tables() ->
    call(index_tables).
schedulers() ->
    call(schedulers).

%%%-----------------------------------------------------------------
%%% Called when a link to a process (Pid) is clicked.
proc_details(Pid) ->
    call({proc_details,Pid}).

%%%-----------------------------------------------------------------
%%% Called when a link to a port is clicked.
port(Id) ->
    call({port,Id}).

%%%-----------------------------------------------------------------
%%% Called when "<< xxx bytes>>" link is clicket to open a new window
%%% displaying the whole binary.
expand_binary(Pos) ->
    call({expand_binary,Pos}).

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
    ets:new(cdv_dump_index_table,[ordered_set,named_table,public]),
    ets:new(cdv_reg_proc_table,[ordered_set,named_table,public]),
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
handle_call(general_info,_From,State=#state{file=File}) ->
    GenInfo = general_info(File),
    NumAtoms = GenInfo#general_info.num_atoms,
    WS = parse_vsn_str(GenInfo#general_info.system_vsn,4),
    TW = case get(truncated) of
	     true -> ["WARNING: The crash dump is truncated. "
		      "Some information might be missing."];
	     false -> []
	 end,
    ets:insert(cdv_reg_proc_table,
	       {cdv_dump_node_name,GenInfo#general_info.node_name}),
    {reply,{ok,GenInfo,TW},State#state{wordsize=WS, num_atoms=NumAtoms}};
handle_call({expand_binary,{Offset,Size,Pos}},_From,State=#state{file=File}) ->
    Fd = open(File),
    pos_bof(Fd,Pos),
    {Bin,_Line} = get_binary(Offset,Size,val(Fd)),
    close(Fd),
    {reply,{ok,Bin},State};
handle_call(procs_summary,_From,State=#state{file=File,wordsize=WS}) ->
    TW = truncated_warning([?proc]),
    Procs = procs_summary(File,WS),
    {reply,{ok,Procs,TW},State};
handle_call({proc_details,Pid},_From,
	    State=#state{file=File,wordsize=WS,dump_vsn=DumpVsn,binaries=B})->
    Reply = 
	case get_proc_details(File,Pid,WS,DumpVsn,B) of
	    {ok,Proc,TW} ->
		{ok,Proc,TW};
	    Other ->
		{error,Other}
	end,
    {reply, Reply, State};
handle_call({port,Id},_From,State=#state{file=File}) ->
    Reply = 
	case get_port(File,Id) of
	    {ok,PortInfo} ->
		TW = truncated_warning([{?port,Id}]),
		{ok,PortInfo,TW};
	    Other ->
		{error,Other}
	end,
    {reply,Reply,State};
handle_call(ports,_From,State=#state{file=File}) ->
    TW = truncated_warning([?port]),
    Ports = get_ports(File),
    {reply,{ok,Ports,TW},State};
handle_call({ets_tables,Pid0},_From,State=#state{file=File,wordsize=WS}) ->
    Pid =
	case Pid0 of
	    all -> '$2';
	    _ -> Pid0
	end,
    TW = truncated_warning([?ets]),
    Ets = get_ets_tables(File,Pid,WS),
    {reply,{ok,Ets,TW},State};
handle_call(internal_ets_tables,_From,State=#state{file=File,wordsize=WS}) ->
    InternalEts = get_internal_ets_tables(File,WS),
    TW = truncated_warning([?internal_ets]),
    {reply,{ok,InternalEts,TW},State};
handle_call({timers,Pid0},_From,State=#state{file=File}) ->
    Pid =
	case Pid0 of
	    all -> '$2';
	    _ -> Pid0
	end,
    TW = truncated_warning([?timer]),
    Timers = get_timers(File,Pid),
    {reply,{ok,Timers,TW},State};
handle_call(dist_info,_From,State=#state{file=File}) ->
    TW = truncated_warning([?visible_node,?hidden_node,?not_connected]),
    Nods=nods(File),
    {reply,{ok,Nods,TW},State};
handle_call({node_info,Channel},_From,State=#state{file=File}) ->
    Reply =
	case get_node(File,Channel) of
	    {ok,Nod} ->
		TW = truncated_warning([?visible_node,
					?hidden_node,
					?not_connected]),
		{ok,Nod,TW};
	    {error,Other} ->
		{error,Other}
	end,
    {reply,Reply,State};
handle_call(loaded_mods,_From,State=#state{file=File}) ->
    TW = truncated_warning([?mod]),
    {_CC,_OC,Mods} = loaded_mods(File),
    {reply,{ok,Mods,TW},State};
handle_call({loaded_mod_details,Mod},_From,State=#state{file=File}) ->
    TW = truncated_warning([{?mod,Mod}]),
    ModInfo = get_loaded_mod_details(File,Mod),
    {reply,{ok,ModInfo,TW},State};
handle_call(funs,_From,State=#state{file=File}) ->
    TW = truncated_warning([?fu]),
    Funs = funs(File),
    {reply,{ok,Funs,TW},State};
handle_call(atoms,_From,State=#state{file=File,num_atoms=NumAtoms0}) ->
    TW = truncated_warning([?atoms]),
    NumAtoms = try list_to_integer(NumAtoms0) catch error:badarg -> -1 end,
    Atoms = atoms(File,NumAtoms),
    {reply,{ok,Atoms,TW},State};
handle_call(memory,_From,State=#state{file=File}) ->
    Memory=memory(File),
    TW = truncated_warning([?memory]),
    {reply,{ok,Memory,TW},State};
handle_call(allocated_areas,_From,State=#state{file=File}) ->
    AllocatedAreas=allocated_areas(File),
    TW = truncated_warning([?allocated_areas]),
    {reply,{ok,AllocatedAreas,TW},State};
handle_call(allocator_info,_From,State=#state{file=File}) ->
    SlAlloc=allocator_info(File),
    TW = truncated_warning([?allocator]),
    {reply,{ok,SlAlloc,TW},State};
handle_call(hash_tables,_From,State=#state{file=File}) ->
    HashTables=hash_tables(File),
    TW = truncated_warning([?hash_table,?index_table]),
    {reply,{ok,HashTables,TW},State};
handle_call(index_tables,_From,State=#state{file=File}) ->
    IndexTables=index_tables(File),
    TW = truncated_warning([?hash_table,?index_table]),
    {reply,{ok,IndexTables,TW},State};
handle_call(schedulers,_From,State=#state{file=File}) ->
    Schedulers=schedulers(File),
    TW = truncated_warning([?scheduler]),
    {reply,{ok,Schedulers,TW},State}.



%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({read_file,File}, _State) ->
    case do_read_file(File) of
	{ok,Binaries,DumpVsn} ->
	    observer_lib:report_progress({ok,done}),
	    {noreply, #state{file=File,binaries=Binaries,dump_vsn=DumpVsn}};
	Error ->
	    end_progress(Error),
	    {noreply, #state{}}
    end;
handle_cast(stop,State) ->
    {stop,normal,State}.


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

%% Read and report progress
progress_read(Fd) ->
    {R,Bytes} =
	case read(Fd) of
	    {ok,Bin} ->
		{{ok,Bin},byte_size(Bin)};
	    Other ->
		{Other,0}
	end,
    update_progress(Bytes),
    R.

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
    val(Fd, "-1").
val(Fd, NoExist) ->
    case get_rest_of_line(Fd) of
	{eof,[]} -> NoExist;
	[] -> NoExist;
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

get_lines_to_empty(Fd) ->
    case get_chunk(Fd) of
	{ok,Bin} ->
	    get_lines_to_empty(Fd,Bin,[],[]);
	eof ->
	    []
    end.
get_lines_to_empty(Fd,<<$\n:8,Bin/binary>>,[],Lines) ->
    put_chunk(Fd,Bin),
    lists:reverse(Lines);
get_lines_to_empty(Fd,<<$\n:8,Bin/binary>>,Acc,Lines) ->
    get_lines_to_empty(Fd,Bin,[],[lists:reverse(Acc)|Lines]);
get_lines_to_empty(Fd,<<$\r:8,Bin/binary>>,Acc,Lines) ->
    get_lines_to_empty(Fd,Bin,Acc,Lines);
get_lines_to_empty(Fd,<<$\s:8,Bin/binary>>,[],Lines) ->
    get_lines_to_empty(Fd,Bin,[],Lines);
get_lines_to_empty(Fd,<<Char:8,Bin/binary>>,Acc,Lines) ->
    get_lines_to_empty(Fd,Bin,[Char|Acc],Lines);
get_lines_to_empty(Fd,<<>>,Acc,Lines) ->
    case get_chunk(Fd) of
	{ok,Bin} ->
	    get_lines_to_empty(Fd,Bin,Acc,Lines);
	eof ->
	    lists:reverse(Lines,[lists:reverse(Acc)])
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

%%%-----------------------------------------------------------------
%%% 
parse_vsn_str([],WS) ->
    WS;
parse_vsn_str(Str,WS) ->
    case Str of
	"[64-bit]" ++ _Rest ->
	    8;
	[_Char|Rest] ->
	    parse_vsn_str(Rest,WS)
    end.


%%%-----------------------------------------------------------------
%%% Traverse crash dump and insert index in table for each heading
%%%
%%% Progress is reported during the time and MUST be checked with
%%% crashdump_viewer:get_progress/0 until it returns {ok,done}.
do_read_file(File) ->
    case file:read_file_info(File) of
	{ok,#file_info{type=regular,
		       access=FileA,
		       size=Size}} when FileA=:=read; FileA=:=read_write ->
	    Fd = open(File),
	    init_progress("Reading file",Size),
	    case progress_read(Fd) of
		{ok,<<$=:8,TagAndRest/binary>>} ->
		    {Tag,Id,Rest,N1} = tag(Fd,TagAndRest,1),
		    case Tag of
			?erl_crash_dump ->
			    reset_index_table(),
			    insert_index(Tag,Id,N1+1),
			    put(last_tag,{Tag,""}),
			    indexify(Fd,Rest,N1),
			    end_progress(),
			    check_if_truncated(),
			    [{DumpVsn0,_}] = lookup_index(?erl_crash_dump),
			    DumpVsn = [list_to_integer(L) ||
					L<-string:tokens(DumpVsn0,".")],
			    Binaries = read_binaries(Fd,DumpVsn),
			    close(Fd),
			    {ok,Binaries,DumpVsn};
			_Other ->
			    R = io_lib:format(
				  "~s is not an Erlang crash dump~n",
				  [File]),
			    close(Fd),
			    {error,R}
		    end;
		{ok,<<"<Erlang crash dump>",_Rest/binary>>} -> 
		    %% old version - no longer supported
		    R = io_lib:format(
			  "The crashdump ~s is in the pre-R10B format, "
			  "which is no longer supported.~n",
			     [File]),
		    close(Fd),
		    {error,R};
		_Other ->
		    R = io_lib:format(
			  "~s is not an Erlang crash dump~n",
			  [File]),
		    close(Fd),
		    {error,R}
	    end;
	_other ->
	    R = io_lib:format("~s is not an Erlang crash dump~n",[File]),
	    {error,R}
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
	    case progress_read(Fd) of
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
    case progress_read(Fd) of
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
		       Tag==?proc_stack;
		       Tag==?proc_heap ->
    true;
is_proc_tag(_) ->
    false.

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

    GI = get_general_info(Fd,#general_info{created=Created}),

    {MemTot,MemMax} = 
	case lookup_index(?memory) of
	    [{_,MemStart}] ->
		pos_bof(Fd,MemStart),
		Memory = get_meminfo(Fd,[]),
		Tot = case lists:keysearch(total,1,Memory) of
			  {value,{_,T}} -> T;
			  false -> ""
		      end,
		Max = case lists:keysearch(maximum,1,Memory) of
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
		    [_] -> "'nonode@nohost'";
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
	"Calling Thread" ->
	    get_general_info(Fd,GenInfo#general_info{thread=val(Fd)});
	"=" ++ _next_tag ->
	    GenInfo;
	Other ->
	    unexpected(Fd,Other,"general information"),
	    GenInfo
    end.

count() ->
    {count_index(?proc),count_index(?ets),count_index(?fu),count_index(?timer)}.


%%-----------------------------------------------------------------
%% Page with all processes
procs_summary(File,WS) ->
    ParseFun = fun(Fd,Pid0) ->
		       Pid = list_to_pid(Pid0),
		       Proc = get_procinfo(Fd,fun main_procinfo/5,
					   #proc{pid=Pid},WS),
		       case Proc#proc.name of
			   undefined ->
			       true;
			   Name ->
			       %% Registered process - store to allow
			       %% lookup for timers connected to
			       %% registered name instead of pid.
			       ets:insert(cdv_reg_proc_table,{Name,Pid}),
			       ets:insert(cdv_reg_proc_table,{Pid0,Name})
		       end,
		       case Proc#proc.memory of
			   undefined -> Proc#proc{memory=Proc#proc.stack_heap};
			   _ -> Proc
		       end
	       end,
    lookup_and_parse_index(File,?proc,ParseFun,"processes").

%%-----------------------------------------------------------------
%% Page with one process
get_proc_details(File,Pid,WS,DumpVsn,Binaries) ->
    case lookup_index(?proc,Pid) of
	[{_,Start}] ->
	    Fd = open(File),
	    {{Stack,MsgQ,Dict},TW} =
		case truncated_warning([{?proc,Pid}]) of
		    [] ->
			{expand_memory(Fd,Pid,DumpVsn,Binaries),[]};
		    TW0 ->
			{{[],[],[]},TW0}
		end,
	    pos_bof(Fd,Start),
	    Proc0 = #proc{pid=Pid,stack_dump=Stack,msg_q=MsgQ,dict=Dict},
	    Proc = get_procinfo(Fd,fun all_procinfo/5,Proc0,WS),
	    close(Fd),
	    {ok,Proc,TW};
	_ ->
	    maybe_other_node(Pid)
    end.

get_procinfo(Fd,Fun,Proc,WS) ->
    case line_head(Fd) of
	"State" ->
	    State = case val(Fd) of
			"Garbing" -> "Garbing\n(limited info)";
			State0 -> State0
		    end,
	    get_procinfo(Fd,Fun,Proc#proc{state=State},WS);
	"Name" ->
	    get_procinfo(Fd,Fun,Proc#proc{name=val(Fd)},WS);
	"Spawned as" ->
	    IF = val(Fd),
	    case Proc#proc.name of
		undefined ->
		    get_procinfo(Fd,Fun,Proc#proc{name=IF,init_func=IF},WS);
		_ ->
		    get_procinfo(Fd,Fun,Proc#proc{init_func=IF},WS)
	    end;
	"Message queue length" ->
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{msg_q_len=list_to_integer(val(Fd))},WS);
	"Reductions" ->
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{reds=list_to_integer(val(Fd))},WS);
	"Stack+heap" ->
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{stack_heap=
					  list_to_integer(val(Fd))*WS},WS);
	"Memory" ->
	    %% stored as integer so we can sort on it
	    get_procinfo(Fd,Fun,Proc#proc{memory=list_to_integer(val(Fd))},WS);
	{eof,_} ->
	    Proc; % truncated file
	Other ->
	    Fun(Fd,Fun,Proc,WS,Other)
    end.

main_procinfo(Fd,Fun,Proc,WS,LineHead) ->
    case LineHead of
	"=" ++ _next_tag ->
	    Proc;
	"arity = " ++ _ ->
	    %%! Temporary workaround
	    get_procinfo(Fd,Fun,Proc,WS);
	_Other ->
	    skip_rest_of_line(Fd),
	    get_procinfo(Fd,Fun,Proc,WS)
    end.
all_procinfo(Fd,Fun,Proc,WS,LineHead) ->
    case LineHead of
	%% - START - moved from get_procinfo -
	"Spawned by" ->
	    case val(Fd) of
		"[]" ->
		    get_procinfo(Fd,Fun,Proc,WS);
		Parent ->
		    get_procinfo(Fd,Fun,Proc#proc{parent=Parent},WS)
	    end;
	"Started" ->
	    get_procinfo(Fd,Fun,Proc#proc{start_time=val(Fd)},WS);
	"Last scheduled in for" ->
	    get_procinfo(Fd,Fun,Proc#proc{current_func=
					  {"Last scheduled in for",
					   val(Fd)}},WS);
	"Current call" ->
	    get_procinfo(Fd,Fun,Proc#proc{current_func={"Current call",
							val(Fd)}},WS);
	"Number of heap fragments" ->
	    get_procinfo(Fd,Fun,Proc#proc{num_heap_frag=val(Fd)},WS);
	"Heap fragment data" ->
	    get_procinfo(Fd,Fun,Proc#proc{heap_frag_data=val(Fd)},WS);
	"OldHeap" ->
	    Bytes = list_to_integer(val(Fd))*WS,
	    get_procinfo(Fd,Fun,Proc#proc{old_heap=Bytes},WS);
	"Heap unused" ->
	    Bytes = list_to_integer(val(Fd))*WS,
	    get_procinfo(Fd,Fun,Proc#proc{heap_unused=Bytes},WS);
	"OldHeap unused" ->
	    Bytes = list_to_integer(val(Fd))*WS,
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_unused=Bytes},WS);
	"New heap start" ->
	    get_procinfo(Fd,Fun,Proc#proc{new_heap_start=val(Fd)},WS);
	"New heap top" ->
	    get_procinfo(Fd,Fun,Proc#proc{new_heap_top=val(Fd)},WS);
	"Stack top" ->
	    get_procinfo(Fd,Fun,Proc#proc{stack_top=val(Fd)},WS);
	"Stack end" ->
	    get_procinfo(Fd,Fun,Proc#proc{stack_end=val(Fd)},WS);
	"Old heap start" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_start=val(Fd)},WS);
	"Old heap top" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_top=val(Fd)},WS);
	"Old heap end" ->
	    get_procinfo(Fd,Fun,Proc#proc{old_heap_end=val(Fd)},WS);
	%% - END - moved from get_procinfo -
	"Last calls" ->
	    get_procinfo(Fd,Fun,Proc#proc{last_calls=get_lines_to_empty(Fd)},WS);
	"Link list" ->
	    {Links,Monitors,MonitoredBy} = parse_link_list(val(Fd),[],[],[]),
	    get_procinfo(Fd,Fun,Proc#proc{links=Links,
					  monitors=Monitors,
					  mon_by=MonitoredBy},WS);
	"Program counter" ->
	    get_procinfo(Fd,Fun,Proc#proc{prog_count=val(Fd)},WS);
	"CP" ->
	    get_procinfo(Fd,Fun,Proc#proc{cp=val(Fd)},WS);
	"arity = " ++ Arity ->
	    %%! Temporary workaround
	    get_procinfo(Fd,Fun,Proc#proc{arity=Arity--"\r\n"},WS);
	"Run queue" ->
	    get_procinfo(Fd,Fun,Proc#proc{run_queue=val(Fd)},WS);
	"Internal State" ->
	    get_procinfo(Fd,Fun,Proc#proc{int_state=val(Fd)},WS);
	"=" ++ _next_tag ->
	    Proc;
	Other ->
	    unexpected(Fd,Other,"process info"),
	    get_procinfo(Fd,Fun,Proc,WS)
    end.

parse_link_list([SB|Str],Links,Monitors,MonitoredBy) when SB==$[; SB==$] ->
    parse_link_list(Str,Links,Monitors,MonitoredBy);
parse_link_list("#Port"++_=Str,Links,Monitors,MonitoredBy) ->
    {Link,Rest} = parse_port(Str),
    parse_link_list(Rest,[Link|Links],Monitors,MonitoredBy);
parse_link_list("<"++_=Str,Links,Monitors,MonitoredBy) ->
    {Link,Rest} = parse_pid(Str),
    parse_link_list(Rest,[Link|Links],Monitors,MonitoredBy);
parse_link_list("{to,"++Str,Links,Monitors,MonitoredBy) ->
    {Mon,Rest} = parse_monitor(Str),
    parse_link_list(Rest,Links,[Mon|Monitors],MonitoredBy);
parse_link_list("{from,"++Str,Links,Monitors,MonitoredBy) ->
    {Mon,Rest} = parse_monitor(Str),
    parse_link_list(Rest,Links,Monitors,[Mon|MonitoredBy]);
parse_link_list(", "++Rest,Links,Monitors,MonitoredBy) ->
    parse_link_list(Rest,Links,Monitors,MonitoredBy);
parse_link_list([],Links,Monitors,MonitoredBy) ->
    {lists:reverse(Links),lists:reverse(Monitors),lists:reverse(MonitoredBy)}.

parse_port(Str) ->
    {Port,Rest} = parse_link(Str,[]),
    {{Port,Port},Rest}.

parse_pid(Str) ->
    {Pid,Rest} = parse_link(Str,[]),
    {{Pid,Pid},Rest}.

parse_monitor("{"++Str) ->
    %% Named process
    {Name,Node,Rest1} = parse_name_node(Str,[]),
    Pid = get_pid_from_name(Name,Node),
    case parse_link(string:strip(Rest1,left,$,),[]) of
	{Ref,"}"++Rest2} ->
	    %% Bug in break.c - prints an extra "}" for remote
	    %% nodes... thus the strip
	    {{Pid,"{"++Name++","++Node++"} ("++Ref++")"},
	     string:strip(Rest2,left,$})};
	{Ref,[]} ->
	    {{Pid,"{"++Name++","++Node++"} ("++Ref++")"},[]}
    end;
parse_monitor(Str) ->
    case parse_link(Str,[]) of
	{Pid,","++Rest1} ->
	    case parse_link(Rest1,[]) of
		{Ref,"}"++Rest2} ->
		    {{Pid,Pid++" ("++Ref++")"},Rest2};
		{Ref,[]} ->
		    {{Pid,Pid++" ("++Ref++")"},[]}
	    end;
	{Pid,[]} ->
	    {{Pid,Pid++" (unknown_ref)"},[]}
    end.

parse_link(">"++Rest,Acc) ->
    {lists:reverse(Acc,">"),Rest};
parse_link([H|T],Acc) ->
    parse_link(T,[H|Acc]);
parse_link([],Acc) ->
    %% truncated
    {lists:reverse(Acc),[]}.

parse_name_node(","++Rest,Name) ->
    parse_name_node(Rest,Name,[]);
parse_name_node([H|T],Name) ->
    parse_name_node(T,[H|Name]);
parse_name_node([],Name) ->
    %% truncated
    {lists:reverse(Name),[],[]}.

parse_name_node("}"++Rest,Name,Node) ->
    {lists:reverse(Name),lists:reverse(Node),Rest};
parse_name_node([H|T],Name,Node) ->
    parse_name_node(T,Name,[H|Node]);
parse_name_node([],Name,Node) ->
    %% truncated
    {lists:reverse(Name),lists:reverse(Node),[]}.

get_pid_from_name(Name,Node) ->
    case ets:lookup(cdv_reg_proc_table,cdv_dump_node_name) of
	[{_,Node}] ->
	    case ets:lookup(cdv_reg_proc_table,Name) of
		[{_,Pid}] when is_pid(Pid) ->
		    pid_to_list(Pid);
		_ ->
		    "<unkonwn_pid>"
	    end;
	_ ->
	    "<unknown_pid_other_node>"
    end.

maybe_other_node(Id) ->
    Channel = 
	case split($.,Id) of
	    {"<" ++ N, _Rest} ->
		N;
	    {"#Port<" ++ N, _Rest} ->
		N;
	    {_, []} ->
		not_found
	end,
    maybe_other_node2(Channel).

maybe_other_node2(not_found) -> not_found;
maybe_other_node2(Channel) ->
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
	[_] ->
	    {other_node,Channel}
    end.


expand_memory(Fd,Pid,DumpVsn,Binaries) ->
    BinAddrAdj = get_bin_addr_adj(DumpVsn),
    put(fd,Fd),
    Dict = read_heap(Fd,Pid,BinAddrAdj,Binaries),
    Expanded = {read_stack_dump(Fd,Pid,BinAddrAdj,Dict),
		read_messages(Fd,Pid,BinAddrAdj,Dict),
		read_dictionary(Fd,Pid,BinAddrAdj,Dict)},
    erase(fd),
    Expanded.

%%%-----------------------------------------------------------------
%%% This is a workaround for a bug in dump versions prior to 0.3:
%%% Addresses were truncated to 32 bits. This could cause binaries to
%%% get the same address as heap terms in the dump. To work around it
%%% we always store binaries on very high addresses in the gb_tree.
get_bin_addr_adj(DumpVsn) when DumpVsn < [0,3] ->
    16#f bsl 64;
get_bin_addr_adj(_) ->
    0.

%%%
%%% Read binaries.
%%%
read_binaries(Fd,DumpVsn) ->
    AllBinaries = lookup_index(?binary),
    AddrAdj = get_bin_addr_adj(DumpVsn),
    Fun = fun({Addr0,Pos},Dict0) ->
		  pos_bof(Fd,Pos),
		  {HexAddr,_} = get_hex(Addr0),
		  Addr = HexAddr bor AddrAdj,
		  Bin =
		      case line_head(Fd) of
			  {eof,_} -> '#CDVTruncatedBinary';
			  _Size   -> {'#CDVBin',Pos}
		      end,
		  gb_trees:enter(Addr,Bin,Dict0)
	  end,
    progress_foldl("Processing binaries",Fun,gb_trees:empty(),AllBinaries).

%%%
%%% Read top level section.
%%%

read_stack_dump(Fd,Pid,BinAddrAdj,Dict) ->
    case lookup_index(?proc_stack,Pid) of
	[{_,Start}] ->
	    pos_bof(Fd,Start),
	    read_stack_dump1(Fd,BinAddrAdj,Dict,[]);
	[] ->
	    []
    end.
read_stack_dump1(Fd,BinAddrAdj,Dict,Acc) ->
    %% This function is never called if the dump is truncated in {?proc_heap,Pid}
    case val(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	Line ->
	    Stack = parse_top(Line,BinAddrAdj,Dict),
	    read_stack_dump1(Fd,BinAddrAdj,Dict,[Stack|Acc])
    end.

parse_top(Line0, BinAddrAdj, D) ->
    {Label,Line1} = get_label(Line0),
    {Term,Line,D} = parse_term(Line1, BinAddrAdj, D),
    [] = skip_blanks(Line),
    {Label,Term}.

%%%
%%% Read message queue.
%%%

read_messages(Fd,Pid,BinAddrAdj,Dict) ->
    case lookup_index(?proc_messages,Pid) of
	[{_,Start}] ->
	    pos_bof(Fd,Start),
	    read_messages1(Fd,BinAddrAdj,Dict,[]);
	[] ->
	    []
    end.
read_messages1(Fd,BinAddrAdj,Dict,Acc) ->
    %% This function is never called if the dump is truncated in {?proc_heap,Pid}
    case val(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	Line ->
	    Msg = parse_message(Line,BinAddrAdj,Dict),
	    read_messages1(Fd,BinAddrAdj,Dict,[Msg|Acc])
    end.
    
parse_message(Line0, BinAddrAdj, D) ->
    {Msg,":"++Line1,_} = parse_term(Line0, BinAddrAdj, D),
    {Token,Line,_} = parse_term(Line1, BinAddrAdj, D),
    [] = skip_blanks(Line),
    {Msg,Token}.
    
%%%
%%% Read process dictionary
%%%

read_dictionary(Fd,Pid,BinAddrAdj,Dict) ->
    case lookup_index(?proc_dictionary,Pid) of
	[{_,Start}] ->
	    pos_bof(Fd,Start),
	    read_dictionary1(Fd,BinAddrAdj,Dict,[]);
	[] ->
	    []
    end.
read_dictionary1(Fd,BinAddrAdj,Dict,Acc) ->
    %% This function is never called if the dump is truncated in {?proc_heap,Pid}
    case val(Fd) of
	"=" ++ _next_tag ->
	    lists:reverse(Acc);
	Line ->
	    Msg = parse_dictionary(Line,BinAddrAdj,Dict),
	    read_dictionary1(Fd,BinAddrAdj,Dict,[Msg|Acc])
    end.
    
parse_dictionary(Line0, BinAddrAdj, D) ->
    {Entry,Line,_} = parse_term(Line0, BinAddrAdj, D),
    [] = skip_blanks(Line),
    Entry.
    
%%%
%%% Read heap data.
%%%

read_heap(Fd,Pid,BinAddrAdj,Dict0) ->
    case lookup_index(?proc_heap,Pid) of
	[{_,Pos}] ->
	    pos_bof(Fd,Pos),
	    read_heap(BinAddrAdj,Dict0);
	[] ->
	    Dict0
    end.

read_heap(BinAddrAdj,Dict0) ->
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
		    Dict = parse(Line,BinAddrAdj,Dict0),
		    read_heap(BinAddrAdj,Dict)
	    end
    end.

parse(Line0, BinAddrAdj, Dict0) ->
    {Addr,":"++Line1} = get_hex(Line0),
    {_Term,Line,Dict} = parse_heap_term(Line1, Addr, BinAddrAdj, Dict0),
    [] = skip_blanks(Line),
    Dict.


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
	    maybe_other_node(Port)
    end.

%%-----------------------------------------------------------------
%% Page with all ports
get_ports(File) ->
    ParseFun = fun(Fd,Id) -> get_portinfo(Fd,#port{id=port_to_tuple(Id)}) end,
    lookup_and_parse_index(File,?port,ParseFun,"ports").

%% Converting port string to tuple to secure correct sorting. This is
%% converted back in cdv_port_cb:format/1.
port_to_tuple("#Port<"++Port) ->
    [I1,I2] = string:tokens(Port,".>"),
    {list_to_integer(I1),list_to_integer(I2)}.

get_portinfo(Fd,Port) ->
    case line_head(Fd) of
	"Slot" ->
	    %% stored as integer so we can sort on it
	    get_portinfo(Fd,Port#port{slot=list_to_integer(val(Fd))});
	"Connected" ->
	    %% stored as pid so we can sort on it
	    Connected0 = val(Fd),
	    Connected =
		try list_to_pid(Connected0)
		catch error:badarg -> Connected0
		end,
	    get_portinfo(Fd,Port#port{connected=Connected});
	"Links" ->
	    Pids = split_pid_list_no_space(val(Fd)),
	    Links = [{Pid,Pid} || Pid <- Pids],
	    get_portinfo(Fd,Port#port{links=Links});
	"Registered as" ->
	    get_portinfo(Fd,Port#port{name=val(Fd)});
	"Monitors" ->
	    Monitors0 = string:tokens(val(Fd),"()"),
	    Monitors = [begin
			    [Pid,Ref] = string:tokens(Mon,","),
			    {Pid,Pid++" ("++Ref++")"}
			end || Mon <- Monitors0],
	    get_portinfo(Fd,Port#port{monitors=Monitors});
	"Port controls linked-in driver" ->
	    Str = lists:flatten(["Linked in driver: " | val(Fd)]),
	    get_portinfo(Fd,Port#port{controls=Str});
	"Port controls forker process" ->
	    Str = lists:flatten(["Forker process: " | val(Fd)]),
	    get_portinfo(Fd,Port#port{controls=Str});
	"Port controls external process" ->
	    Str = lists:flatten(["External proc: " | val(Fd)]),
	    get_portinfo(Fd,Port#port{controls=Str});
	"Port is a file" ->
	    Str = lists:flatten(["File: "| val(Fd)]),
	    get_portinfo(Fd,Port#port{controls=Str});
	"Port is UNIX fd not opened by emulator" ->
	    Str = lists:flatten(["UNIX fd not opened by emulator: "| val(Fd)]),
	    get_portinfo(Fd,Port#port{controls=Str});
	"=" ++ _next_tag ->
	    Port;
	Other ->
	    unexpected(Fd,Other,"port info"),
	    Port
    end.

split_pid_list_no_space(String) ->
    split_pid_list_no_space(String,[],[]).
split_pid_list_no_space([$>|Rest],Acc,Pids) ->
    split_pid_list_no_space(Rest,[],[lists:reverse(Acc,[$>])|Pids]);
split_pid_list_no_space([H|T],Acc,Pids) ->
    split_pid_list_no_space(T,[H|Acc],Pids);
split_pid_list_no_space([],[],Pids) ->
    lists:reverse(Pids).

%%-----------------------------------------------------------------
%% Page with external ets tables
get_ets_tables(File,Pid,WS) ->
    ParseFun = fun(Fd,Id) ->
		       get_etsinfo(Fd,#ets_table{pid=list_to_pid(Id)},WS)
	       end,
    lookup_and_parse_index(File,{?ets,Pid},ParseFun,"ets").

get_etsinfo(Fd,EtsTable = #ets_table{details=Ds},WS) ->
    case line_head(Fd) of
	"Slot" ->
	    get_etsinfo(Fd,EtsTable#ets_table{slot=list_to_integer(val(Fd))},WS);
	"Table" ->
	    get_etsinfo(Fd,EtsTable#ets_table{id=val(Fd)},WS);
	"Name" ->
	    get_etsinfo(Fd,EtsTable#ets_table{name=val(Fd)},WS);
	"Ordered set (AVL tree), Elements" ->
	    skip_rest_of_line(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{data_type="tree"},WS);
	"Buckets" ->
	    %% A bug in erl_db_hash.c prints a space after the buckets
	    %% - need to strip the string to make list_to_integer/1 happy.
	    Buckets = list_to_integer(string:strip(val(Fd))),
	    get_etsinfo(Fd,EtsTable#ets_table{buckets=Buckets},WS);
	"Objects" ->
	    get_etsinfo(Fd,EtsTable#ets_table{size=list_to_integer(val(Fd))},WS);
	"Words" ->
	    Words = list_to_integer(val(Fd)),
	    Bytes = 
		case Words of
		    -1 -> -1; % probably truncated
		    _ -> Words * WS
		end,
	    get_etsinfo(Fd,EtsTable#ets_table{memory={bytes,Bytes}},WS);
	"=" ++ _next_tag ->
	    EtsTable;
	"Chain Length Min" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{chain_min=>Val}},WS);
	"Chain Length Avg" ->
	    Val = try list_to_float(string:strip(val(Fd))) catch _:_ -> "-" end,
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{chain_avg=>Val}},WS);
	"Chain Length Max" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{chain_max=>Val}},WS);
	"Chain Length Std Dev" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{chain_stddev=>Val}},WS);
	"Chain Length Expected Std Dev" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{chain_exp_stddev=>Val}},WS);
	"Fixed" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{fixed=>Val}},WS);
	"Type" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{data_type=Val},WS);
	"Protection" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{protection=>Val}},WS);
	"Compressed" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{compressed=>Val}},WS);
	"Write Concurrency" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{write_c=>Val}},WS);
	"Read Concurrency" ->
	    Val = val(Fd),
	    get_etsinfo(Fd,EtsTable#ets_table{details=Ds#{read_c=>Val}},WS);
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
get_timers(File,Pid) ->
    ParseFun = fun(Fd,Id) -> get_timerinfo(Fd,Id) end,
    T1 = lookup_and_parse_index(File,{?timer,Pid},ParseFun,"timers"),
    T2 = case ets:lookup(cdv_reg_proc_table,Pid) of
	     [{_,Name}] ->
		 lookup_and_parse_index(File,{?timer,Name},ParseFun,"timers");
	     _ ->
		 []
	 end,
    T1 ++ T2.

get_timerinfo(Fd,Id) ->
    case catch list_to_pid(Id) of
	Pid when is_pid(Pid) ->
	    get_timerinfo_1(Fd,#timer{pid=Pid});
	_ ->
	    case ets:lookup(cdv_reg_proc_table,Id) of
		[{_,Pid}] when is_pid(Pid) ->
		    get_timerinfo_1(Fd,#timer{pid=Pid,name=Id});
		[] ->
		    get_timerinfo_1(Fd,#timer{name=Id})
	    end
    end.

get_timerinfo_1(Fd,Timer) ->
    case line_head(Fd) of
	"Message" ->
	    get_timerinfo_1(Fd,Timer#timer{msg=val(Fd)});
	"Time left" ->
	    TimeLeft = list_to_integer(val(Fd) -- " ms"),
	    get_timerinfo_1(Fd,Timer#timer{time=TimeLeft});
	"=" ++ _next_tag ->
	    Timer;
	Other ->
	    unexpected(Fd,Other,"timer info"),
	    Timer
    end.

%%-----------------------------------------------------------------
%% Page with information about a node in the distribution
get_node(File,Channel) ->
    Ms = ets:fun2ms(
	   fun({{Tag,Start},Ch}) when Tag=:=?visible_node, Ch=:=Channel ->
		   {visible,Start};
	      ({{Tag,Start},Ch}) when Tag=:=?hidden_node, Ch=:=Channel ->
		   {hidden,Start};
	      ({{Tag,Start},Ch}) when Tag=:=?not_connected, Ch=:=Channel ->
		   {not_connected,Start}
	   end),

    case ets:select(cdv_dump_index_table,Ms) of
	[] ->
	    {error,not_found};
	[{Type,Pos}] ->
	    Fd = open(File),
	    NodeInfo = get_nodeinfo(Fd,Channel,Type,Pos),
	    close(Fd),
	    {ok,NodeInfo}
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
				get_nodeinfo(Fd,Channel,visible,Start)
			end, 
			V),
	    Hidden = lists:map(
		       fun({Channel,Start}) -> 
			       get_nodeinfo(Fd,Channel,hidden,Start)
		       end, 
		       H),
	    NotConnected = lists:map(
			     fun({Channel,Start}) -> 
				     get_nodeinfo(Fd,Channel,not_connected,Start)
			     end, 
			     N),
	    close(Fd),
	    Visible++Hidden++NotConnected;
	[_] ->
	    %% no_distribution
	    []
    end.

get_nodeinfo(Fd,Channel,Type,Start) ->
    pos_bof(Fd,Start),
    get_nodeinfo(Fd,#nod{channel=list_to_integer(Channel),conn_type=Type}).

get_nodeinfo(Fd,Nod) ->
    case line_head(Fd) of
	"Name" ->
	    get_nodeinfo(Fd,Nod#nod{name=val(Fd)});
	"Controller" ->
	    get_nodeinfo(Fd,Nod#nod{controller=val(Fd)});
	"Creation" ->
	    %% Throwing away elements like "(refc=1)", which might be
	    %% printed from a debug compiled emulator.
	    Creations = lists:flatmap(fun(C) -> try [list_to_integer(C)]
						catch error:badarg -> []
						end
				      end, string:tokens(val(Fd)," ")),
	    get_nodeinfo(Fd,Nod#nod{creation={creations,Creations}});
	"Remote link" ->
	    Procs = val(Fd), % e.g. "<0.31.0> <4322.54.0>"
	    {Local,Remote} = split(Procs),
	    Str = Local++" <-> "++Remote,
	    NewRemLinks = [{Local,Str} | Nod#nod.remote_links],
	    get_nodeinfo(Fd,Nod#nod{remote_links=NewRemLinks});
	"Remote monitoring" ->
	    Procs = val(Fd), % e.g. "<0.31.0> <4322.54.0>"
	    {Local,Remote} = split(Procs),
	    Str = Local++" -> "++Remote,
	    NewRemMon = [{Local,Str} | Nod#nod.remote_mon],
	    get_nodeinfo(Fd,Nod#nod{remote_mon=NewRemMon});
	"Remotely monitored by" ->
	    Procs = val(Fd), % e.g. "<0.31.0> <4322.54.0>"
	    {Local,Remote} = split(Procs),
	    Str = Local++" <- "++Remote,
	    NewRemMonBy = [{Local,Str} | Nod#nod.remote_mon_by],
	    get_nodeinfo(Fd,Nod#nod{remote_mon_by=NewRemMonBy});
	"Error" ->
	    get_nodeinfo(Fd,Nod#nod{error="ERROR: "++val(Fd)});
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
loaded_mods(File) ->
    ParseFun = 
	fun(Fd,Id) -> 
		get_loaded_mod_info(Fd,
				    #loaded_mod{mod=get_atom(list_to_binary(Id))},
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
    {CC,OC,lookup_and_parse_index(File,?mod,ParseFun,"modules")}.

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
	    CS = list_to_integer(val(Fd)),
	    get_loaded_mod_info(Fd,LM#loaded_mod{current_size=CS},Fun);
	"Old size" ->
	    OS = list_to_integer(val(Fd)),
	    get_loaded_mod_info(Fd,LM#loaded_mod{old_size=OS},Fun);
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
funs(File) ->
    ParseFun = fun(Fd,_Id) -> get_funinfo(Fd,#fu{}) end,
    lookup_and_parse_index(File,?fu,ParseFun,"funs").

get_funinfo(Fd,Fu) ->
    case line_head(Fd) of
	"Module" ->
	    get_funinfo(Fd,Fu#fu{module=val(Fd)});
	"Uniq" ->
	    get_funinfo(Fd,Fu#fu{uniq=list_to_integer(val(Fd))});
	"Index" ->
	    get_funinfo(Fd,Fu#fu{index=list_to_integer(val(Fd))});
	"Address" ->
	    get_funinfo(Fd,Fu#fu{address=val(Fd)});
	"Native_address" ->
	    get_funinfo(Fd,Fu#fu{native_address=val(Fd)});
	"Refc" ->
	    get_funinfo(Fd,Fu#fu{refc=list_to_integer(val(Fd))});
	"=" ++ _next_tag ->
	    Fu;
	Other ->
	    unexpected(Fd,Other,"fun info"),
	    Fu
    end.

%%-----------------------------------------------------------------
%% Page with list of all atoms
atoms(File,NumAtoms) ->
    case lookup_index(?atoms) of
	[{_Id,Start}] ->
	    Fd = open(File),
	    pos_bof(Fd,Start),
	    get_atoms(Fd,NumAtoms);
	_ ->
	    []
    end.

get_atoms(Fd,NumAtoms) ->
    case get_chunk(Fd) of
	{ok,Bin} ->
	    init_progress("Processing atoms",NumAtoms),
	    get_atoms(Fd,Bin,NumAtoms,[]);
	eof ->
	    []
    end.


%% Atoms are written one per line in the crash dump, in creation order
%% from last to first.
get_atoms(Fd,Bin,NumAtoms,Atoms) ->
    Bins = binary:split(Bin,<<"\n">>,[global]),
    get_atoms1(Fd,Bins,NumAtoms,Atoms).

get_atoms1(_Fd,[<<"=",_/binary>>|_],_N,Atoms) ->
    end_progress(),
    Atoms;
get_atoms1(Fd,[LastBin],N,Atoms) ->
    case get_chunk(Fd) of
	{ok,Bin0} ->
	    get_atoms(Fd,<<LastBin/binary,Bin0/binary>>,N,Atoms);
	eof ->
	    end_progress(),
	    [{N,get_atom(LastBin)}|Atoms]
    end;
get_atoms1(Fd,[Bin|Bins],N,Atoms) ->
    update_progress(),
    get_atoms1(Fd,Bins,N-1,[{N,get_atom(Bin)}|Atoms]).

%% This ensures sorting according to first actual letter in the atom,
%% disregarding possible single quote. It is formatted back to correct
%% syntax in cdv_atom_cb:format/1
get_atom(<<"\'",Atom/binary>>) ->
    {Atom,q}; % quoted
get_atom(Atom) when is_binary(Atom) ->
    {Atom,nq}. % not quoted

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
	    get_meminfo(Fd,[{list_to_atom(Key),val(Fd)}|Acc])
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
			{Key,Alloc,""};
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
    get_allocatorinfo1(Fd,[],0).

get_allocatorinfo1(Fd,Acc,Max) ->
    case line_head(Fd) of
	"=" ++ _next_tag ->
	    pad_and_reverse(Acc,Max,[]);
	{eof,_last_line} ->
	    pad_and_reverse(Acc,Max,[]);
	Key ->
	    Values = get_all_vals(val(Fd),[]),
	    L = length(Values),
	    Max1 = if L > Max -> L; true -> Max end,
	    get_allocatorinfo1(Fd,[{Key,Values}|Acc],Max1)
    end.
	    
get_all_vals([$ |Rest],Acc) ->
    [lists:reverse(Acc)|get_all_vals(Rest,[])];
get_all_vals([],Acc) ->
    [lists:reverse(Acc)];
get_all_vals([Char|Rest],Acc) ->
    get_all_vals(Rest,[Char|Acc]).

%% Make sure all V have the same length by padding with "".
pad_and_reverse([{K,V}|T],Len,Rev) ->
    VLen = length(V),
    V1 = if VLen == Len -> V;
	    true -> V ++ lists:duplicate(Len-VLen,"")
	 end,
    pad_and_reverse(T,Len,[{K,V1}|Rev]);
pad_and_reverse([],_,Rev) ->
    Rev.

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
    {"Allocator Summary",
     ["blocks size","carriers size","mseg carriers size"],
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
%% Page with scheduler table information
schedulers(File) ->
    case lookup_index(?scheduler) of
	[] ->
	    [];
	Schedulers ->
	    Fd = open(File),
	    R = lists:map(fun({Name,Start}) ->
				  get_schedulerinfo(Fd,Name,Start)
			  end,
			  Schedulers),
	    close(Fd),
	    R
    end.

get_schedulerinfo(Fd,Name,Start) ->
    pos_bof(Fd,Start),
    get_schedulerinfo1(Fd,#sched{name=Name}).

get_schedulerinfo1(Fd,Sched=#sched{details=Ds}) ->
    case line_head(Fd) of
	"Current Process" ->
	    get_schedulerinfo1(Fd,Sched#sched{process=val(Fd, "None")});
	"Current Port" ->
	    get_schedulerinfo1(Fd,Sched#sched{port=val(Fd, "None")});
	"Run Queue Max Length" ->
	    RQMax = list_to_integer(val(Fd)),
	    RQ = RQMax + Sched#sched.run_q,
	    get_schedulerinfo1(Fd,Sched#sched{run_q=RQ, details=Ds#{runq_max=>RQMax}});
	"Run Queue High Length" ->
	    RQHigh = list_to_integer(val(Fd)),
	    RQ = RQHigh + Sched#sched.run_q,
	    get_schedulerinfo1(Fd,Sched#sched{run_q=RQ, details=Ds#{runq_high=>RQHigh}});
	"Run Queue Normal Length" ->
	    RQNorm = list_to_integer(val(Fd)),
	    RQ = RQNorm + Sched#sched.run_q,
	    get_schedulerinfo1(Fd,Sched#sched{run_q=RQ, details=Ds#{runq_norm=>RQNorm}});
	"Run Queue Low Length" ->
	    RQLow = list_to_integer(val(Fd)),
	    RQ = RQLow + Sched#sched.run_q,
	    get_schedulerinfo1(Fd,Sched#sched{run_q=RQ, details=Ds#{runq_low=>RQLow}});
	"Run Queue Port Length" ->
	    RQ = list_to_integer(val(Fd)),
	    get_schedulerinfo1(Fd,Sched#sched{port_q=RQ});

	"Scheduler Sleep Info Flags" ->
	    get_schedulerinfo1(Fd,Sched#sched{details=Ds#{sleep_info=>val(Fd, "None")}});
	"Scheduler Sleep Info Aux Work" ->
	    get_schedulerinfo1(Fd,Sched#sched{details=Ds#{sleep_aux=>val(Fd, "None")}});

	"Run Queue Flags" ->
	    get_schedulerinfo1(Fd,Sched#sched{details=Ds#{runq_flags=>val(Fd, "None")}});

	"Current Process State" ->
	    get_schedulerinfo1(Fd,Sched#sched{details=Ds#{currp_state=>val(Fd)}});
	"Current Process Internal State" ->
	    get_schedulerinfo1(Fd,Sched#sched{details=Ds#{currp_int_state=>val(Fd)}});
	"Current Process Program counter" ->
	    get_schedulerinfo1(Fd,Sched#sched{details=Ds#{currp_prg_cnt=>val(Fd)}});
	"Current Process CP" ->
	    get_schedulerinfo1(Fd,Sched#sched{details=Ds#{currp_cp=>val(Fd)}});
	"Current Process Limited Stack Trace" ->
	    %% If there shall be last in scheduler information block
	    Sched#sched{details=get_limited_stack(Fd, 0, Ds)};
	"=" ++ _next_tag ->
	    Sched;
	Other ->
	    unexpected(Fd,Other,"scheduler information"),
	    Sched
    end.

get_limited_stack(Fd, N, Ds) ->
    case val(Fd) of
	Addr = "0x" ++ _ ->
	    get_limited_stack(Fd, N+1, Ds#{{currp_stack, N} => Addr});
	"=" ++ _next_tag ->
	    Ds;
	Line ->
	    get_limited_stack(Fd, N+1, Ds#{{currp_stack, N} => Line})
    end.

%%%-----------------------------------------------------------------
%%% Parse memory in crashdump version 0.1 and newer
%%%
parse_heap_term([$l|Line0], Addr, BinAddrAdj, D0) ->	%Cons cell.
    {H,"|"++Line1,D1} = parse_term(Line0, BinAddrAdj, D0),
    {T,Line,D2} = parse_term(Line1, BinAddrAdj, D1),
    Term = [H|T],
    D = gb_trees:insert(Addr, Term, D2),
    {Term,Line,D};
parse_heap_term([$t|Line0], Addr, BinAddrAdj, D) ->	%Tuple
    {N,":"++Line} = get_hex(Line0),
    parse_tuple(N, Line, Addr, BinAddrAdj, D, []);
parse_heap_term([$F|Line0], Addr, _BinAddrAdj, D0) ->	%Float
    {N,":"++Line1} = get_hex(Line0),
    {Chars,Line} = get_chars(N, Line1),
    Term = list_to_float(Chars),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("B16#"++Line0, Addr, _BinAddrAdj, D0) -> %Positive big number.
    {Term,Line} = get_hex(Line0),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("B-16#"++Line0, Addr, _BinAddrAdj, D0) -> %Negative big number
    {Term0,Line} = get_hex(Line0),
    Term = -Term0,
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("B"++Line0, Addr, _BinAddrAdj, D0) ->	%Decimal big num
    case string:to_integer(Line0) of
	{Int,Line} when is_integer(Int) ->
	    D = gb_trees:insert(Addr, Int, D0),
	    {Int,Line,D}
    end;
parse_heap_term([$P|Line0], Addr, _BinAddrAdj, D0) ->	% External Pid.
    {Pid0,Line} = get_id(Line0),
    Pid = ['#CDVPid'|Pid0],
    D = gb_trees:insert(Addr, Pid, D0),
    {Pid,Line,D};
parse_heap_term([$p|Line0], Addr, _BinAddrAdj, D0) ->   % External Port.
    {Port0,Line} = get_id(Line0),
    Port = ['#CDVPort'|Port0],
    D = gb_trees:insert(Addr, Port, D0),
    {Port,Line,D};
parse_heap_term("E"++Line0, Addr, _BinAddrAdj, D0) ->	%Term encoded in external format.
    {Bin,Line} = get_binary(Line0),
    Term = binary_to_term(Bin),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("Yh"++Line0, Addr, _BinAddrAdj, D0) ->	%Heap binary.
    {Term,Line} = get_binary(Line0),
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("Yc"++Line0, Addr, BinAddrAdj, D0) ->	%Reference-counted binary.
    {Binp0,":"++Line1} = get_hex(Line0),
    {Offset,":"++Line2} = get_hex(Line1),
    {Sz,Line} = get_hex(Line2),
    Binp = Binp0 bor BinAddrAdj,
    Term = case gb_trees:lookup(Binp, D0) of
	       {value,Bin} -> cdvbin(Offset,Sz,Bin);
	       none -> '#CDVNonexistingBinary'
	   end,
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D};
parse_heap_term("Ys"++Line0, Addr, BinAddrAdj, D0) ->	%Sub binary.
    {Binp0,":"++Line1} = get_hex(Line0),
    {Offset,":"++Line2} = get_hex(Line1),
    {Sz,Line} = get_hex(Line2),
    Binp = Binp0 bor BinAddrAdj,
    Term = case gb_trees:lookup(Binp, D0) of
	       {value,Bin} -> cdvbin(Offset,Sz,Bin);
	       none when Binp0=/=Binp ->
		   %% Might it be on the heap?
		   case gb_trees:lookup(Binp0, D0) of
		       {value,Bin} -> cdvbin(Offset,Sz,Bin);
		       none -> '#CDVNonexistingBinary'
		   end;
	       none -> '#CDVNonexistingBinary'
	   end,
    D = gb_trees:insert(Addr, Term, D0),
    {Term,Line,D}.


parse_tuple(0, Line, Addr, _, D0, Acc) ->
    Tuple = list_to_tuple(lists:reverse(Acc)),
    D = gb_trees:insert(Addr, Tuple, D0),
    {Tuple,Line,D};
parse_tuple(N, Line0, Addr, BinAddrAdj, D0, Acc) ->
    case parse_term(Line0, BinAddrAdj, D0) of
	{Term,[$,|Line],D} when N > 1 ->
	    parse_tuple(N-1, Line, Addr, BinAddrAdj, D, [Term|Acc]);
	{Term,Line,D}->
	    parse_tuple(N-1, Line, Addr, BinAddrAdj, D, [Term|Acc])
    end.

parse_term([$H|Line0], BinAddrAdj, D) ->        %Pointer to heap term.
    {Ptr,Line} = get_hex(Line0),
    deref_ptr(Ptr, Line, BinAddrAdj, D);
parse_term([$N|Line], _, D) ->			%[] (nil).
    {[],Line,D};
parse_term([$I|Line0], _, D) ->			%Small.
    {Int,Line} = string:to_integer(Line0),
    {Int,Line,D};
parse_term([$A|_]=Line, _, D) ->		%Atom.
    parse_atom(Line, D);
parse_term([$P|Line0], _, D) ->			%Pid.
    {Pid,Line} = get_id(Line0),
    {['#CDVPid'|Pid],Line,D};
parse_term([$p|Line0], _, D) ->			%Port.
    {Port,Line} = get_id(Line0),
    {['#CDVPort'|Port],Line,D};
parse_term([$S|Str0], _, D) ->			%Information string.
    Str = lists:reverse(skip_blanks(lists:reverse(Str0))),
    {Str,[],D};
parse_term([$D|Line0], _, D) ->                 %DistExternal
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
    
    

deref_ptr(Ptr, Line, BinAddrAdj, D0) ->
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
			    deref_ptr(Ptr, Line, BinAddrAdj, D0);
			L ->
			    D = parse(L, BinAddrAdj, D0),
			    deref_ptr(Ptr, Line, BinAddrAdj, D)
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

get_id(Line0) ->
    [$<|Line] = lists:dropwhile(fun($<) -> false; (_) -> true end,Line0),
    get_id(Line, [], []).

get_id([$>|Line], Acc, Id) ->
    {lists:reverse(Id,[list_to_integer(lists:reverse(Acc))]),Line};
get_id([$.|Line], Acc, Id) ->
    get_id(Line,[],[list_to_integer(lists:reverse(Acc))|Id]);
get_id([H|T], Acc, Id) ->
    get_id(T, [H|Acc], Id).

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
    do_get_binary(N, Line, []).

get_binary(Offset,Size,Line0) ->
    {_N,":"++Line} = get_hex(Line0),
    do_get_binary(Size, lists:sublist(Line,(Offset*2)+1,Size*2), []).

do_get_binary(0, Line, Acc) ->
    {list_to_binary(lists:reverse(Acc)),Line};
do_get_binary(N, [A,B|Line], Acc) ->
    Byte = (get_hex_digit(A) bsl 4) bor get_hex_digit(B),
    do_get_binary(N-1, Line, [Byte|Acc]);
do_get_binary(_N, [], _Acc) ->
    {'#CDVTruncatedBinary',[]}.

cdvbin(Offset,Size,{'#CDVBin',Pos}) ->
    ['#CDVBin',Offset,Size,Pos];
cdvbin(Offset,Size,['#CDVBin',_,_,Pos]) ->
    ['#CDVBin',Offset,Size,Pos];
cdvbin(_,_,'#CDVTruncatedBinary') ->
    '#CDVTruncatedBinary'.

%%-----------------------------------------------------------------
%% Functions for accessing the cdv_dump_index_table
reset_index_table() ->
    ets:delete_all_objects(cdv_dump_index_table).

insert_index(Tag,Id,Pos) ->
    ets:insert(cdv_dump_index_table,{{Tag,Pos},Id}).

lookup_index({Tag,Id}) ->
    lookup_index(Tag,Id);
lookup_index(Tag) ->
    lookup_index(Tag,'$2').
lookup_index(Tag,Id) ->
    ets:select(cdv_dump_index_table,[{{{Tag,'$1'},Id},[],[{{Id,'$1'}}]}]).

count_index(Tag) ->
    ets:select_count(cdv_dump_index_table,[{{{Tag,'_'},'_'},[],[true]}]).


%%-----------------------------------------------------------------
%% Convert tags read from crashdump to atoms used as first part of key
%% in cdv_dump_index_table
tag_to_atom("allocated_areas") -> ?allocated_areas;
tag_to_atom("allocator") -> ?allocator;
tag_to_atom("atoms") -> ?atoms;
tag_to_atom("binary") -> ?binary;
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
tag_to_atom("old_instr_data") -> ?old_instr_data;
tag_to_atom("port") -> ?port;
tag_to_atom("proc") -> ?proc;
tag_to_atom("proc_dictionary") -> ?proc_dictionary;
tag_to_atom("proc_heap") -> ?proc_heap;
tag_to_atom("proc_messages") -> ?proc_messages;
tag_to_atom("proc_stack") -> ?proc_stack;
tag_to_atom("scheduler") -> ?scheduler;
tag_to_atom("timer") -> ?timer;
tag_to_atom("visible_node") -> ?visible_node;
tag_to_atom(UnknownTag) ->
    io:format("WARNING: Found unexpected tag:~s~n",[UnknownTag]),
    list_to_atom(UnknownTag).

%%%-----------------------------------------------------------------
%%% Fetch next chunk from crashdump file
lookup_and_parse_index(File,What,ParseFun,Str) when is_list(File) ->
    Indices = lookup_index(What),
    Fun  = fun(Fd,{Id,Start}) ->
		   pos_bof(Fd,Start),
		   ParseFun(Fd,Id)
	   end,
    Report = "Processing " ++ Str,
    progress_pmap(Report,File,Fun,Indices).

%%%-----------------------------------------------------------------
%%% Convert a record to a proplist
to_proplist(Fields,Record) ->
    Values = to_value_list(Record),
    lists:zip(Fields,Values).

%%%-----------------------------------------------------------------
%%% Convert a record to a simple list of field values
to_value_list(Record) ->
    [_RecordName|Values] = tuple_to_list(Record),
    Values.

%%%-----------------------------------------------------------------
%%% Fold over List and report progress in percent.
%%% Report is the text to be presented in the progress dialog.
%%% Acc0 is the initial accumulator and will be passed to Fun as the
%%% second arguement, i.e. Fun = fun(Item,Acc) -> NewAcc end.
progress_foldl(Report,Fun,Acc0,List) ->
    init_progress(Report, length(List)),
    progress_foldl1(Fun,Acc0,List).

progress_foldl1(Fun,Acc,[H|T]) ->
    update_progress(),
    progress_foldl1(Fun,Fun(H,Acc),T);
progress_foldl1(_Fun,Acc,[]) ->
    end_progress(),
    Acc.


%%%-----------------------------------------------------------------
%%% Map over List and report progress in percent.
%%% Report is the text to be presented in the progress dialog.
%%% Distribute the load over a number of processes, and File is opened
%%% on each process and passed to the Fun as first argument.
%%% I.e. Fun = fun(Fd,Item) -> ItemResult end.
progress_pmap(Report,File,Fun,List) ->
    NTot = length(List),
    NProcs = erlang:system_info(schedulers) * 2,
    NPerProc = (NTot div NProcs) + 1,

    %% Worker processes send message to collector for each ReportInterval.
    ReportInterval = (NTot div 100) + 1,

    %% Progress reporter on collector process reports 1 percent for
    %% each message from worker process.
    init_progress(Report,99),

    Collector = self(),
    {[],Pids} =
	lists:foldl(
	  fun(_,{L,Ps}) ->
		  {L1,L2} = if length(L)>=NPerProc -> lists:split(NPerProc,L);
			       true -> {L,[]} % last chunk
			    end,
		  {P,_Ref} =
		      spawn_monitor(
			fun() ->
				progress_map(Collector,ReportInterval,File,Fun,L1)
			end),
		  {L2,[P|Ps]}
	  end,
	  {List,[]},
	  lists:seq(1,NProcs)),
    collect(Pids,[]).

progress_map(Collector,ReportInterval,File,Fun,List) ->
    Fd = open(File),
    init_progress(ReportInterval, fun(_) -> Collector ! progress end, ok),
    progress_map(Fd,Fun,List,[]).
progress_map(Fd,Fun,[H|T],Acc) ->
    update_progress(),
    progress_map(Fd,Fun,T,[Fun(Fd,H)|Acc]);
progress_map(Fd,_Fun,[],Acc) ->
    close(Fd),
    exit({pmap_done,Acc}).

collect([],Acc) ->
    end_progress(),
    lists:append(Acc);
collect(Pids,Acc) ->
    receive
	progress ->
	    update_progress(),
	    collect(Pids,Acc);
	{'DOWN', _Ref, process, Pid, {pmap_done,Result}} ->
	    collect(lists:delete(Pid,Pids),[Result|Acc])
    end.

%%%-----------------------------------------------------------------
%%% Help functions for progress reporting

%% Set text in progress dialog and initialize the progress counter
init_progress(Report,N) ->
    observer_lib:report_progress({ok,Report}),
    Interval = (N div 100) + 1,
    Fun = fun(P0) -> P=P0+1,observer_lib:report_progress({ok,P}),P end,
    init_progress(Interval,Fun,0).
init_progress(Interval,Fun,Acc) ->
    put(progress,{Interval,Interval,Fun,Acc}),
    ok.

%% Count progress and report on given interval
update_progress() ->
    update_progress(1).
update_progress(Processed) ->
    do_update_progress(get(progress),Processed).

do_update_progress({Count,Interval,Fun,Acc},Processed) when Processed>Count ->
    do_update_progress({Interval,Interval,Fun,Fun(Acc)},Processed-Count);
do_update_progress({Count,Interval,Fun,Acc},Processed) ->
    put(progress,{Count-Processed,Interval,Fun,Acc}),
    ok.

%% End progress reporting for this item
end_progress() ->
    end_progress({ok,100}).
end_progress(Report) ->
    observer_lib:report_progress(Report),
    erase(progress),
    ok.
