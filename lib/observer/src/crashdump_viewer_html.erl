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
-module(crashdump_viewer_html).

%% 
%% This module implements the HTML generation for the crashdump
%% viewer. No logic or states are kept by this module.
%%

-export([welcome/0,
	 read_file_frame/0,
	 redirect/1,
	 start_page/0,
	 filename_frame/1,
	 menu_frame/0,
	 general_info/1,
	 pretty_info_page/2,
	 info_page/2,
	 procs_summary/4,
	 proc_details/4,
	 expanded_memory/2,
	 expanded_binary/1,
	 next/2,
	 ports/3,
	 timers/3,
	 ets_tables/4,
	 nods/2,
	 loaded_mods/2,
	 loaded_mod_details/2,
	 funs/2,
	 atoms/3,
	 memory/2,
	 allocated_areas/2,
	 allocator_info/2,
	 hash_tables/2,
	 index_tables/2,
	 error/2]).

-include("crashdump_viewer.hrl").

%%%-----------------------------------------------------------------
%%% Welcome frame
welcome() ->
    header(body(welcome_body())).

welcome_body() ->
    table(
      "WIDTH=100% HEIGHT=60%",
      [tr("VALIGN=middle",
	  td("ALIGN=center",
	     font("SIZE=6",
		  ["Welcome to the Web Based",br(),
		   "Erlang Crash Dump Analyser"]))),
       tr("VALIGN=middle",
	  td("ALIGN=center",
	     form(["name=load_new ACTION=\"./read_file_frame\""],
		  input(["TYPE=submit VALUE=\"Load Crashdump\""]))))]).

%%%-----------------------------------------------------------------
%%% Present a form to enter file name of erlang crash dump
read_file_frame() ->
    header("Read File",body(read_file_frame_body())).


read_file_frame_body() ->
    Entry =
	case webtool:is_localhost() of 
	    true -> [input("TYPE=file NAME=browse SIZE=40"),
		     input("TYPE=hidden NAME=path")];
	    false -> input("TYPE=text NAME=path SIZE=60")
	end,
    Form = 
	form(
	  "NAME=read_file_form METHOD=post ACTION= \"./read_file\"",
	  table(
	    "BORDER=0",
	    [tr(td("COLSPAN=2","Enter file to analyse")),
	     tr(
	       [td(Entry),
		td("ALIGN=center",
		   input("TYPE=submit onClick=\"path.value=browse.value;\""
			 "VALUE=Ok"))])])),
    table(
      "WIDTH=100% HEIGHT=60%",
      tr("VALIGN=middle",
	 td("ALIGN=center",Form))).


%%%-----------------------------------------------------------------
%%% Display "Please wait..." while crashdump is being read
redirect(Status) ->
    Head = ["<META HTTP-EQUIV=\"refresh\" CONTENT=\"3; URL=./redirect\">"],
    header("Please wait...",Head,body([Status,br(),"Please wait..."])).

%%%-----------------------------------------------------------------
%%% Frameset containing "filename", "menu", and "main" frames
start_page() ->
    header("Crashdump Viewer Start Page",start_page_frameset()).

start_page_frameset() ->
    frameset(
      "ROWS=\"70,*\"",
      [frame(["NAME=\"filename\" SRC=\"./filename_frame\""]),
       frameset(
	 "COLS=\"200,*\"",
	 [frame(["NAME=\"menu\" ",
		 "SRC=\"/cdv_erl/crashdump_viewer/menu_frame\""]),
	  frame("NAME=\"main\" SRC=\"./initial_info_frame\"")])]).



%%%-----------------------------------------------------------------
%%% Topmost frame presents the filename of the crashdump currently
%%% viewed
filename_frame(File) ->
    header("Filename",body(filename_body(File))).

filename_body(File) ->
    p("ALIGN=center",[b("Crashdump currently viewed:"),br(),File]).
    

%%%-----------------------------------------------------------------
%%% Left frame displays the menu
menu_frame() ->
    header("Menu", body(menu_body())).

menu_body() ->
    [p(format_items(1,ets:info(cdv_menu_table,size),true)), 
     p([br(),
	form(["name=load_new ACTION=\"./read_file_frame\" ",
	      "TARGET=app_frame"],
	     input("TYPE=submit VALUE=\"Load New Crashdump\""))])].

format_items(I,Max,_ParentState) when I>Max->
    [];
format_items(I,Max,ParentState) when I=<Max->
    case ets:lookup(cdv_menu_table,I) of
	[] -> [];
	[#menu_item{state=false,children=0}] ->
	    format_items(I+1,Max,ParentState);
	[#menu_item{state=false,children=Children}] ->
	    format_items(I+Children+1,Max,arentState);
	[Item=#menu_item{state=true,children=0}] when ParentState ->
	    This = format_item(Item),
	    [This|format_items(I+1,Max,ParentState)];
	[Item=#menu_item{state=true,children=Children}] when ParentState ->
	    This = format_item(Item),
	    Ch = format_items(I+1,I+Children,true),
	    [[This | Ch] | format_items(I+Children+1,Max,ParentState)]
    end.

format_item(Item) ->
    [lists:duplicate(Item#menu_item.depth*5,?space),
     format_picture(Item#menu_item.index,
		    Item#menu_item.picture,
		    Item#menu_item.children), 
     format_title(Item#menu_item.text,Item#menu_item.target), 
     br()].

format_picture(_Index,Picture,0) ->
    img(Picture);
format_picture(Index,Picture,_Children) ->
    href( ["./toggle?index=", integer_to_list(Index)], img(Picture)).

format_title({Link,Text},Target) ->
    href(["TARGET=\"",Target,"\""],Link,Text);
format_title(Text,_Type) ->
    Text.

%%%-----------------------------------------------------------------
%%% Display the general information
general_info(GenInfo) ->
    Heading = "General Information",
    header(Heading,body(general_info_body(Heading,GenInfo))).

general_info_body(Heading,GenInfo) ->
    TruncatedInfo = 
	case get(truncated) of
	    true ->
		p(font("SIZE=\"+1\" COLOR=\"#FF0000\"",
		       b(["WARNING:",br(),
			  "The crashdump is truncated",br(),
			  "Some information might be missing",br()])));
	    false ->
		""
	end,

    [heading(Heading,"general_info"),
     TruncatedInfo,
     table(
       "BORDER=4 CELLPADDING=4",
       [tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Slogan"),
	    td(GenInfo#general_info.slogan)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Node name"),
	    td(GenInfo#general_info.node_name)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Crashdump created on"),
	    td(GenInfo#general_info.created)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","System version"),
	    td(GenInfo#general_info.system_vsn)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Compiled"),
	    td(GenInfo#general_info.compile_time)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Taints"),
	    td(GenInfo#general_info.taints)]),
	case GenInfo#general_info.mem_tot of
	    "" -> "";
	    MemTot ->
		tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Memory allocated"),
		    td([MemTot," bytes"])])
	end,
	case GenInfo#general_info.mem_max of
	    "" -> "";
	    MemMax ->
		tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Memory maximum"),
		    td([MemMax," bytes"])])
	end,
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Atoms"),
	    td(GenInfo#general_info.num_atoms)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Processes"),
	    td(GenInfo#general_info.num_procs)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","ETS tables"),
	    td(GenInfo#general_info.num_ets)]),
	tr([th("ALIGN=left BGCOLOR=\"#8899AA\"","Funs"),
	    td(GenInfo#general_info.num_fun)])]),
     case GenInfo#general_info.instr_info of
	 old_instr_data ->
	     [br(),br(),
	      font("COLOR=\"#FF0000\"",
		   ["Instrumentation information is found at the end of ",br(),
		    "the dump. The information has an old format, and ",br(),
		    "is not presented in this tool. Please read the ",br(),
		    "crashdump manually to see this information."])];
	 instr_data ->
	     [br(),br(),
	      font("COLOR=\"#FF0000\"",
		   ["Instrumentation information is found at the end of ",br(),
		    "the dump. The information is not presented in this ",br(),
		    "tool. Please read the crashdump manually to see",br(),
		    "this information."])];	     
	 false ->
	     []
     end].

%%%-----------------------------------------------------------------
%%% Display an error message
error(Text,Args) ->
    Str = io_lib:format(Text,Args),
    header(body(error_body(Str))).

error_body(Str) ->
    [h1("An error occured:"),Str,"\n"].


%%%-----------------------------------------------------------------
%%% Display the given information as is
info_page(Heading,Info) ->
    info_page(Heading,Info,[]).
info_page(Heading,Info,TW) ->
    header(Heading,body(info_body(Heading,Info,TW))).
    
info_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No information was found\n"];
info_body(Heading,Info,TW) ->
    [h1(Heading),
     warn(TW),
     pre(href_proc_port(lists:flatten(Info)))].
    
%%%-----------------------------------------------------------------
%%% Pretty print the given information
pretty_info_page(Heading,Info) ->
    header(Heading,body(pretty_info_body(Heading,Info))).
    
pretty_info_body(Heading,[]) ->
    [h1(Heading),
     "No information was found\n"];
pretty_info_body(Heading,Info) ->
    [h1(Heading),
     pre(pretty_format(Info))].
    
%%%-----------------------------------------------------------------
%%% Make table with summary of process information
procs_summary(Sorted,ProcsSummary,TW,SharedHeap) ->
    Heading = "Process Information",
    header(Heading,
	   body(
	     procs_summary_body(Heading,ProcsSummary,TW,Sorted,SharedHeap))).
    
procs_summary_body(Heading,[],TW,_Sorted,_SharedHeap) ->
    [h1(Heading),
     warn(TW),
     "No processes were found\n"];
procs_summary_body(Heading,ProcsSummary,TW,Sorted,SharedHeap) ->
    MemHeading = 
	if SharedHeap -> 
		"Stack";
	   true ->
		"Stack+heap"
	end,

    [heading(Heading,"processes"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [summary_table_head("pid","Pid",Sorted),
	   summary_table_head("name_func","Name/Spawned as",Sorted),
	   summary_table_head("state","State",Sorted), 
	   summary_table_head("reds","Reductions",Sorted), 
	   summary_table_head("mem",MemHeading,Sorted),
	   summary_table_head("msg_q_len","MsgQ Length",Sorted)]) |
	lists:map(fun(Proc) -> procs_summary_table(Proc) end,ProcsSummary)])].

summary_table_head(Sorted,Text,Sorted) ->
    %% Mark the sorted column (bigger and italic)
    th(font("SIZE=\"+1\"",em(href("./sort_procs?sort="++Sorted,Text))));
summary_table_head(SortOn,Text,_Sorted) ->
    th(href("./sort_procs?sort="++SortOn,Text)).

procs_summary_table(Proc) ->
    #proc{pid=Pid,name=Name,state=State,
	  reds=Reds,stack_heap=Mem0,msg_q_len=MsgQLen}=Proc,
    Mem = case Mem0 of
	      -1 -> "unknown";
	      _ -> integer_to_list(Mem0)
	  end,
    tr(
      [td(href(["./proc_details?pid=",Pid],Pid)),
       td(Name),
       td(State),
       td("ALIGN=right",integer_to_list(Reds)),
       td("ALIGN=right",Mem),
       td("ALIGN=right",integer_to_list(MsgQLen))]).

%%%-----------------------------------------------------------------
%%% Print details for one process
proc_details(Pid,Proc,TW,SharedHeap) ->
    Script = 
"<SCRIPT type=\"text/javascript\">
   function popup() {
     window.open(\"\",\"expanded\",'resizable=yes,scrollbars=yes')
}  
</SCRIPT>\n",
    
    Heading = ["Process ", Pid],
    header(Heading,Script,body(proc_details_body(Heading,Proc,TW,SharedHeap))).

proc_details_body(Heading,Proc,TW,SharedHeap) ->
    Pid = Proc#proc.pid,
    Name = if Proc#proc.name==Proc#proc.init_func -> ?space;
	      true -> Proc#proc.name
	   end,
    [help("processes"),
     warn(TW),
     table(
       "BORDER=4 COLS=4 WIDTH=\"100%\"",
       [tr(
	  "BGCOLOR=\"#8899AA\"",
	  [td("COLSPAN=4 ALIGN=center",Heading)]),
	tr(
	  [td("NOWRAP=true",b("Name")),
	   td("COLSPAN=1",Name),
	   td("NOWRAP=true",b("Spawned as")),
	   td("COLSPAN=1",Proc#proc.init_func)]),
	tr(
	  [td("NOWRAP=true",b("State")),
	   td("COLSPAN=1",Proc#proc.state),
	   td("NOWRAP=true",b(element(1,Proc#proc.current_func))),
	   td("COLSPAN=1",element(2,Proc#proc.current_func))]),
	tr(
	  [td("NOWRAP=true",b("Started")),
	   td("COLSPAN=1",Proc#proc.start_time),
	   td("NOWRAP=true",b("Spawned by")),
	   td("COLSPAN=1",href_proc_port(Proc#proc.parent))]),
	tr(
	  [td("NOWRAP=true",b("Reductions")),
	   td("COLSPAN=3",integer_to_list(Proc#proc.reds))]),
	if SharedHeap ->
		Stack = case Proc#proc.stack_heap of
			    -1 -> "unknown";
			    S -> integer_to_list(S)
			end,
		tr(
		  [td("NOWRAP=true",b("Stack")),
		   td("COLSPAN=3",Stack)]);
	   true ->
		[tr(
		   [td("NOWRAP=true",b("Stack+heap")),
		    td(integer_to_list(Proc#proc.stack_heap)),
		    td("NOWRAP=true",b("OldHeap")),
		    td(Proc#proc.old_heap)]),
		 tr(
		   [td("NOWRAP=true",b("Heap unused")),
		    td(Proc#proc.heap_unused),
		    td("NOWRAP=true",b("OldHeap unused")),
		    td(Proc#proc.old_heap_unused)]),
		 tr(
		   [td("NOWRAP=true",b("Number of heap fragments")),
		    td(Proc#proc.num_heap_frag),
		    td("NOWRAP=true",b("Heap fragment data")),
		    td(Proc#proc.heap_frag_data)])]
	end,
	case Proc#proc.new_heap_start of
	    ?space -> "";
	    _ ->
		%% Garbing
		[tr(
		   [td("NOWRAP=true",b("New heap start")),
		    td("COLSPAN=1",Proc#proc.new_heap_start),
		    td("NOWRAP=true",b("New heap top")),
		    td("COLSPAN=1",Proc#proc.new_heap_top)]),
		 tr(
		   [td("NOWRAP=true",b("Stack top")),
		    td("COLSPAN=1",Proc#proc.stack_top),
		    td("NOWRAP=true",b("Stack end")),
		    td("COLSPAN=1",Proc#proc.stack_end)]),
		 tr(
		   [td("NOWRAP=true",b("Old heap start")),
		    td("COLSPAN=1",Proc#proc.old_heap_start),
		    td("NOWRAP=true",b("Old heap top")),
		    td("COLSPAN=1",Proc#proc.old_heap_top)]),
		 tr(
		   [td("NOWRAP=true",b("Old heap end")),
		    td("COLSPAN=3",Proc#proc.old_heap_end)])]
	end,
	case Proc#proc.prog_count of
	    ?space -> "";
	    _ ->
		[tr(
		   [td("NOWRAP=true",b("Program counter")),
		    td("COLSPAN=3",Proc#proc.prog_count)]),
		 tr(
		   [td("NOWRAP=true",b("Continuation pointer")),
		    td("COLSPAN=3",Proc#proc.cp)]),
		 tr(
		   [td("NOWRAP=true",b("Arity")),
		    td("COLSPAN=3",Proc#proc.arity)])]
	end,
	tr(
	  [td("NOWRAP=true",b("Link list")),
	   td("COLSPAN=3",href_proc_port(Proc#proc.links))]),

	tr(
	  [td("NOWRAP=true",b("Msg queue length")),
	   td("COLSPAN=3",integer_to_list(Proc#proc.msg_q_len))]),

	%% These are displayed only if data exist
	display_or_link_to_expand("MsgQueue",Proc#proc.msg_q,Pid),
	display_or_link_to_expand("Dictionary",Proc#proc.dict,Pid),
	display_or_link_to_expand("DebugDictionary",Proc#proc.debug_dict,Pid),
	display_or_link_to_expand("LastCalls",Proc#proc.last_calls,Pid),
	display_or_link_to_expand("StackDump",Proc#proc.stack_dump,Pid)]),

     p([href(["./ets_tables?pid=",Proc#proc.pid],
	     "ETS tables owned by this process"),
	"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
	href(["./timers?pid=",Proc#proc.pid],
	     "Timers owned by this process")])].

display_or_link_to_expand(Heading,Data,Pid) ->
    case Data of
	expand ->
	    link_to_read_memory(Heading,Pid);	    
 	truncated -> 
 	    Text = font("COLOR=\"#FF0000\"",
			"The dump is truncated, no data available"),
 	    tr(
 	      [td("NOWRAP=true VALIGN=top",b(Heading)),
 	       td("COLSPAN=3",Text)]);
	?space -> 
	    "";
	{size,Truncated,Size,Pos} ->
	    %% Too much data, or truncated data - 
	    %% display a link to expand it
	    tr(
	      [td("NOWRAP=true",b(Heading)),
	       td("COLSPAN=3",
		  href("TARGET=\"expanded\" onClick=popup()",
		       ["./expand?pos=",integer_to_list(Pos),
			"&size=",integer_to_list(Size),
			"&what=",Heading,
			"&truncated=",atom_to_list(Truncated)],
		       ["Expand (",integer_to_list(Size)," bytes)"]))]);
	_ ->
	    %% Not too much Data - display it
	    tr(
	      [td("NOWRAP=true VALIGN=top",b(Heading)),
	       td("COLSPAN=3",pre(format(Heading,Data)))])
    end.

link_to_read_memory(Heading,Pid) ->
    tr(
      [td("NOWRAP=true",b(Heading)),
       td("COLSPAN=3",
	  href("TARGET=\"expanded\" onClick=popup()",
	       ["./expand_memory?pid=",Pid,
		"&what=",Heading],
	       ["Expand ", Heading]))]).

format("LastCalls",Data) ->
    Data;
format("StackDump",Data) ->
    Data;
format(_Heading,Data) ->
    pretty_format(Data).
    


%%%-----------------------------------------------------------------
%%% Expanded memory
expanded_memory(Heading,Expanded) ->
    header(Heading,body(expanded_memory_body(Heading,Expanded))).
    
expanded_memory_body(Heading,[]) ->
    [heading(Heading,"processes"),
     case Heading of
	 "MsgQueue" -> "No messages were found";
	 "StackDump" -> "No stack dump was found";
	 "Dictionary" -> "No dictionary was found";
	 "DebugDictionary" -> "No debug dictionary was found"
     end];
expanded_memory_body(Heading,Expanded) ->
    [heading(Heading,"processes"),
     case Heading of
	 "MsgQueue" ->
	     table(
	       "BORDER=4 CELLPADDING=4",
	       [tr(
		  [th("Message"),
		   th("SeqTraceToken")]) |
		lists:map(fun(Msg) -> msgq_table(Msg) end, Expanded)]);
	 "StackDump" ->
	     table(
	       "BORDER=4 CELLPADDING=4",
	       [tr(
		  [th("Label"),
		   th("Term")]) |
		lists:map(fun(Entry) -> stackdump_table(Entry) end, Expanded)]);
	 _ ->
	     table(
	       "BORDER=4 CELLPADDING=4",
	       [tr(
		  [th("Key"),
		   th("Value")]) |
		lists:map(fun(Entry) -> dict_table(Entry) end, Expanded)])
     end].

msgq_table({Msg0,Token0}) ->
    Token = case Token0 of
		[] -> ?space;
		_ -> io_lib:fwrite("~w",[Token0])
	    end,
    Msg = href_proc_port(lists:flatten(io_lib:format("~p",[Msg0]))),
    tr([td(pre(Msg)), td(Token)]).
    
stackdump_table({Label0,Term0}) ->
    Label = io_lib:format("~w",[Label0]),
    Term = href_proc_port(lists:flatten(io_lib:format("~p",[Term0]))),
    tr([td("VALIGN=top",Label), td(pre(Term))]).
    
dict_table({Key0,Value0}) ->
    Key = href_proc_port(lists:flatten(io_lib:format("~p",[Key0]))),
    Value = href_proc_port(lists:flatten(io_lib:format("~p",[Value0]))),
    tr([td("VALIGN=top",pre(Key)), td(pre(Value))]).


%%%-----------------------------------------------------------------
%%% Display an expanded binary, i.e. the whole binary, not just the
%%% size of it.
expanded_binary(Bin) ->
    Heading = "Expanded binary",
    header(Heading,body(expanded_binary_body(Heading,Bin))).
    
expanded_binary_body(Heading,Bin) ->
    [h1(Heading),
     pre(href_proc_port(lists:flatten(Bin))),
     br(),br(),
     href("javascript:history.go(-1)","BACK")].
    
%%%-----------------------------------------------------------------
%%% Print table of ports
ports(Heading,Ports,TW) ->
    header(Heading,body(ports_body(Heading,Ports,TW))).
    
ports_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No ports were found\n"];
ports_body(Heading,Ports,TW) ->
    [heading(Heading,"ports"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [th("Id"),
	   th("Slot"),
	   th("Connected"),
	   th("Links"),
	   th("Controls")]) |
	lists:map(fun(Port) -> ports_table(Port) end, Ports)])].

ports_table(Port) ->
    #port{id=Id,slot=Slot,connected=Connected,links=Links,
	  controls=Controls}=Port,
    tr(
      [td(Id),
       td("ALIGHT=right",Slot),
       td(href_proc_port(Connected)),
       td(href_proc_port(Links)),
       td(Controls)]).
    
%%%-----------------------------------------------------------------
%%% Print table of ETS tables
ets_tables(Heading,EtsTables,InternalEts,TW) ->
    header(Heading,body(ets_tables_body(Heading,EtsTables,InternalEts,TW))).
    
ets_tables_body(Heading,[],InternalEts,TW) ->
    [h1(Heading),
     warn(TW),
     "No ETS tables were found\n" |
     internal_ets_tables_table(InternalEts)];
ets_tables_body(Heading,EtsTables,InternalEts,TW) ->
    [heading(Heading,"ets_tables"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [th("Owner"),
	   th("Slot"),
	   th("Id"),
	   th("Name"),
	   th("Type"),
	   th("Buckets"),
	   th("Objects"),
	   th("Memory (bytes)")]) |
	lists:map(fun(EtsTable) -> ets_tables_table(EtsTable) end,
		  EtsTables)]) |
    internal_ets_tables_table(InternalEts)].

ets_tables_table(EtsTable) ->
    #ets_table{pid=Pid,slot=Slot,id=Id,name=Name,type=Type,
	       buckets=Buckets,size=Size,memory=Memory} = EtsTable,
    tr(
      [td(href_proc_port(Pid)),
       td(Slot),
       td(Id),
       td(Name),
       td(Type),
       td("ALIGN=right",Buckets),
       td("ALIGN=right",Size),
       td("ALIGN=right",Memory)]).

internal_ets_tables_table(InternalEtsTables) ->
    [h2("Internal ETS tables"),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [th("Description"),
	   th("Id"),
	   th("Name"),
	   th("Type"),
	   th("Buckets"),
	   th("Objects"),
	   th("Memory (bytes)")]) |
	lists:map(fun(InternalEtsTable) -> 
			  internal_ets_tables_table1(InternalEtsTable)
		  end,
		  InternalEtsTables)])].

internal_ets_tables_table1({Descr,InternalEtsTable}) ->
    #ets_table{id=Id,name=Name,type=Type,buckets=Buckets,
	       size=Size,memory=Memory} = InternalEtsTable,
    tr(
      [td(Descr),
       td(Id),
       td(Name),
       td(Type),
       td("ALIGN=right",Buckets),
       td("ALIGN=right",Size),
       td("ALIGN=right",Memory)]).

%%%-----------------------------------------------------------------
%%% Print table of timers
timers(Heading,Timers,TW) ->
    header(Heading,body(timers_body(Heading,Timers,TW))).
    
timers_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No timers were found\n"];
timers_body(Heading,Timers,TW) ->
    [heading(Heading,"timers"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [th("Owner"),
	   th("Message"),
	   th("Time left")]) |
	lists:map(fun(Timer) -> timers_table(Timer) end, Timers)])].

timers_table(Timer) ->
    #timer{pid=Pid,msg=Msg,time=Time}=Timer,
    tr(
      [td(href_proc_port(Pid)),
       td(Msg),
       td("ALIGN=right",Time)]).

%%%-----------------------------------------------------------------
%%% Print table of nodes in distribution
nods(Nods,TW) ->
    header("Distribution Information",body(nodes_body(Nods,TW))).
    
nodes_body(no_distribution,_TW) ->
    [heading("Distribution Information","distribution_info"),
     "Not alive\n"];
nodes_body({Type,Info,Node},TW) when is_record(Node,nod) ->
    %% Display only one node - used when a pid or port on a remote
    %% node is clicked.
    [heading("Remote Node","distribution_info"),
     warn(TW),
     Info,
     make_nodes_table(Type,[Node])];
nodes_body({Visible,Hidden,NotConnected},TW) ->
    %% Display all nodes - this is the complete distribution info
    [heading("Distribution Information","distribution_info"),
     warn(TW),
     make_nodes_table("Visible Nodes",Visible),
     make_nodes_table("Hidden Nodes",Hidden),
     make_nodes_table("Not Connected Nodes",NotConnected)].

make_nodes_table(Text,[]) ->
    p(["No \"",Text,"\" were found"]);
make_nodes_table(Text,Nodes) ->
    p(table(
	"BORDER=4 CELLPADDING=4",
	[nodes_table_heading(Text),
	 lists:map(fun(Node) -> nodes_table_row(Node) end, Nodes)])).
    
nodes_table_heading(Text) ->
    [tr("BGCOLOR=\"#8899AA\"",[th("COLSPAN=6",Text)]),
     tr([th("Name"),
	 th("Channel"),
	 th("Controller"),
	 th("Creation(s)"),
	 th("Links/Monitors"),
	 th("Extra info")])].

nodes_table_row(Node) ->
    #nod{name=Name,channel=Channel,controller=Controller,creation=Creation,
	 remote_links=Links,remote_mon=Mon,remote_mon_by=MonBy,error=Error}=Node,
    tr(
      [td(maybe_refcount(Name)),
       td("ALIGN=right",Channel),
       td(href_proc_port(Controller)),
       td("ALIGN=right",break_lines_creation(Creation)),
       td(format_links_and_monitors(Links,Mon,MonBy)),
       td(format_extra_info(Error))]).

maybe_refcount(Name) ->
    maybe_refcount(Name, []).
maybe_refcount([$ ,$( | Rest], Acc) ->
    [lists:reverse(Acc),br(),[$(|Rest]];
maybe_refcount([Char | Rest], Acc) ->
    maybe_refcount(Rest, [Char | Acc]);
maybe_refcount([],Acc) ->
    lists:reverse(Acc).

break_lines_creation(Creation) ->
    break_lines_creation(Creation,[]).
break_lines_creation([$ ,$( | Rest1], Acc) ->
    {RefCount,Rest2} = to_end_par(Rest1,[$(,$ ]),
    [lists:reverse(Acc),RefCount,br(),break_lines_creation(Rest2)];
break_lines_creation([$ | Rest], Acc) ->
    [lists:reverse(Acc),br(),break_lines_creation(Rest)];    
break_lines_creation([Char | Rest], Acc) ->
    break_lines_creation(Rest, [Char | Acc]);
break_lines_creation([],Acc) ->
    lists:reverse(Acc).

to_end_par([$),$ | Rest], Acc) ->
    {lists:reverse([$) | Acc]),Rest};
to_end_par([$) | Rest], Acc) ->
    {lists:reverse([$) | Acc]),Rest};
to_end_par([Char | Rest], Acc) ->
    to_end_par(Rest, [Char | Acc]);
to_end_par([],Acc) ->
    {lists:reverse(Acc),[]}.
    

format_links_and_monitors(?space,?space,?space) ->
    ?space;
format_links_and_monitors(Links,Mon,MonBy) ->
    [format_links_and_monitors(Links," is linked to "),
     format_links_and_monitors(Mon," is monitoring "),
     format_links_and_monitors(MonBy," is monitored by ")].

format_links_and_monitors(?space,_Text) ->
    "";
format_links_and_monitors([{Local,Remote}|Rest],Text) ->
    [[href_proc_port(Local),Text,href_proc_port(Remote),br()] |
     format_links_and_monitors(Rest,Text)];
format_links_and_monitors([],_Text) ->
    [].

format_extra_info(?space) ->
    ?space;
format_extra_info(Error) ->
     case Error of
	 ?space -> "";
	 _ -> font("COLOR=\"#FF0000\"",["ERROR: ",Error,"\n"])
     end.
%%%-----------------------------------------------------------------
%%% Print loaded modules information
loaded_mods({CC,OC,LM},TW) ->
    Heading = "Loaded Modules Information",
    header(Heading,body(loaded_mods_body(Heading,CC,OC,LM,TW))).
    
loaded_mods_body(Heading,"unknown","unknown",[],TW) ->
    [h1(Heading),
     warn(TW),
     "No loaded modules information was found\n"];
loaded_mods_body(Heading,CC,OC,LM,TW) ->
    [heading(Heading,"loaded_modules"),
     warn(TW),
     p([b("Current code: "),CC," bytes",br(),
	b("Old code: "),OC," bytes"]),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr([th("Module"),
	    th("Current size (bytes)"),
	    th("Old size (bytes)")]) |
	lists:map(fun(Mod) -> loaded_mods_table(Mod) end,LM)])].

loaded_mods_table(#loaded_mod{mod=Mod,current_size=CS,old_size=OS}) ->
    tr([td(href(["loaded_mod_details?mod=",Mod],Mod)),
	td("ALIGN=right",CS),
	td("ALIGN=right",OS)]).
    

%%%-----------------------------------------------------------------
%%% Print detailed information about one module
loaded_mod_details(ModInfo,TW) ->
    header(ModInfo#loaded_mod.mod,body(loaded_mod_details_body(ModInfo,TW))).
    
loaded_mod_details_body(ModInfo,TW) ->
    #loaded_mod{mod=Mod,current_size=CS,current_attrib=CA,
		current_comp_info=CCI,old_size=OS,
		old_attrib=OA,old_comp_info=OCI} = ModInfo,
    [help("loaded_modules"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(th("BGCOLOR=\"#8899AA\" COLSPAN=3",
	      ["Module: ",Mod])),
	tr([td(?space),th("Current"),th("Old")]),
	tr([th("ALIGN=left","Size (bytes)"),
	    td(CS),
	    td(OS)]),
	tr([th("ALIGN=left","Attributes"),
	    td(pre(CA)),
	    td(pre(OA))]),
	tr([th("ALIGN=left","Compilation info"),
	    td(pre(CCI)),
	    td(pre(OCI))])])].
    

%%%-----------------------------------------------------------------
%%% Print table of funs
funs(Funs,TW) ->
    Heading = "Fun Information",
    header(Heading,body(funs_body(Heading,Funs,TW))).
    
funs_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No Fun information was found\n"];
funs_body(Heading,Funs,TW) ->
    [heading(Heading,"funs"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [th("Module"),
	   th("Uniq"),
	   th("Index"),
	   th("Address"),
	   th("Native_address"),
	   th("Refc")]) |
	lists:map(fun(Fun) -> funs_table(Fun) end, Funs)])].

funs_table(Fu) ->
    #fu{module=Module,uniq=Uniq,index=Index,address=Address,
	native_address=NativeAddress,refc=Refc}=Fu,
    tr(
      [td(Module),
       td("ALIGN=right",Uniq),
       td("ALIGN=right",Index),
       td(Address),
       td(NativeAddress),
       td("ALIGN=right",Refc)]).
    
%%%-----------------------------------------------------------------
%%% Print atoms
atoms(Atoms,Num,TW) ->
    Heading = "Atoms",
    header(Heading,body(atoms_body(Heading,Atoms,Num,TW))).

atoms_body(Heading,[],Num,TW) ->
    [h1(Heading),
     warn(TW),
     "No atoms were found in log",br(),
     "Total number of atoms in node was ", Num, br()];    
atoms_body(Heading,Atoms,Num,TW) ->
    [heading(Heading,"atoms"),
     warn(TW),
     "Total number of atoms in node was ", Num, 
     br(),
     "The last created atom is shown first",
     br(),br() |
     n_first(Atoms)].

n_first({n_lines,Start,N,What,Lines,Pos}) ->
    NextHref = next_href(N,What,Pos,Start),
    [What," number ",integer_to_list(Start),"-",integer_to_list(Start+N-1),
     br(),
     NextHref, 
     pre(Lines),
     NextHref];
n_first({n_lines,_Start,_N,_What,Lines}) ->
    [pre(Lines)].

%%%-----------------------------------------------------------------
%%% Print next N lines of "something"
next(NLines,TW) ->
    header(element(4,NLines),body(next_body(NLines,TW))).

next_body({n_lines,Start,N,What,Lines,Pos},TW) ->
    PrefHref = prev_href(),
    NextHref = next_href(N,What,Pos,Start),
    [warn(TW),
     What," number ",integer_to_list(Start),"-",integer_to_list(Start+N-1),
     br(),
     PrefHref,
     ?space,
     NextHref, 
     pre(Lines),
     PrefHref,
     ?space,
     NextHref];
next_body({n_lines,Start,N,What,Lines},TW) ->
    PrefHref = prev_href(),
    [warn(TW),
     What," number ",integer_to_list(Start),"-",integer_to_list(Start+N-1),
     br(),
     PrefHref,
     pre(Lines),
     PrefHref].
    

prev_href() ->
    href("javascript:history.back()",["Previous"]).

next_href(N,What,Pos,Start) ->
    href(["./next?pos=",integer_to_list(Pos),
	  "&num=",integer_to_list(N),
	  "&start=",integer_to_list(Start+N),
	  "&what=",What],
	 "Next").

%%%-----------------------------------------------------------------
%%% Print memory information
memory(Memory,TW) ->
    Heading = "Memory Information",
    header(Heading,body(memory_body(Heading,Memory,TW))).

memory_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No memory information was found\n"];
memory_body(Heading,Memory,TW) ->
    [heading(Heading,"memory"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr("BGCOLOR=\"#8899AA\"",
	  [th(?space),
	   th("Bytes")]) |
	lists:map(fun(Entry) -> memory_table(Entry) end, Memory)])].

memory_table({Key,Value}) ->
    tr([th("ALIGN=left",Key),td("ALIGN=right",Value)]).

%%%-----------------------------------------------------------------
%%% Print allocated areas information
allocated_areas(AllocatedAreas,TW) ->
    Heading = "Information about allocated areas",
    header(Heading,body(allocated_areas_body(Heading,AllocatedAreas,TW))).

allocated_areas_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No information was found about allocated areas\n"];
allocated_areas_body(Heading,AllocatedAreas,TW) ->
    [heading(Heading,"memory"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr("BGCOLOR=\"#8899AA\"",
	  [th(?space),
	   th("Allocated (bytes)"),
	   th("Used (bytes)")]) | 
	lists:map(fun(Entry) -> allocated_areas_table(Entry) end,
		  AllocatedAreas)])].

allocated_areas_table({Key,Alloc,Used}) ->
    tr(
      [th("ALIGN=left",Key),
       td("ALIGN=right",Alloc),
       td("ALIGN=right",Used)]).


%%%-----------------------------------------------------------------
%%% Print allocator_info information
allocator_info(Allocators,TW) ->
    Heading = "Allocator Information",
    header(Heading,body(allocator_info_body(Heading,Allocators,TW))).

allocator_info_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No information was found about allocators\n"];
allocator_info_body(Heading,Allocators,TW) ->
    [heading(Heading,"memory"),
     warn(TW),
     p(b("Sizes are in bytes")),
     lists:map(fun({SubTitle,Allocator}) ->
		       [table(
			  "BORDER=4 CELLPADDING=4",
			  [tr("BGCOLOR=\"#8899AA\"",
			      th("COLSPAN=10 ALIGN=left", 
				 font("SIZE=+1",SubTitle))) | 
			   lists:map(
			     fun({Key,Values}) -> 
				     tr([th("ALIGN=left",Key) |
					 lists:map(
					   fun(Val) ->
						   td("ALIGN=right",Val)
					   end,Values)])
			     end, 
			     Allocator)]),
			br(),br()]
	       end,
	       Allocators)].

%%%-----------------------------------------------------------------
%%% Print informatin about internal tables
hash_tables(HashTables,TW) ->
    Heading = "Hash Table Information",
    header(Heading,body(hash_tables_body(Heading,HashTables,TW))).
    
hash_tables_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No hash table information was found\n"];
hash_tables_body(Heading,HashTables,TW) ->
    [heading(Heading,"internal_tables"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [th("Name"),
	   th("Size"),
	   th("Used"),
	   th("Objects"),
	   th("Depth")]) |
	lists:map(fun(HashTable) -> hash_tables_table(HashTable) end,
		  HashTables)])].

hash_tables_table(HashTable) ->
    #hash_table{name=Name,size=Size,used=Used,objs=Objs,depth=Depth}=HashTable,
    tr(
      [td(Name),
       td("ALIGN=right",Size),
       td("ALIGN=right",Used),
       td("ALIGN=right",Objs),
       td("ALIGN=right",Depth)]).

index_tables(IndexTables,TW) ->
    Heading = "Index Table Information",
    header(Heading,body(index_tables_body(Heading,IndexTables,TW))).
    
index_tables_body(Heading,[],TW) ->
    [h1(Heading),
     warn(TW),
     "No index table information was found\n"];
index_tables_body(Heading,IndexTables,TW) ->
    [heading(Heading,"internal_tables"),
     warn(TW),
     table(
       "BORDER=4 CELLPADDING=4",
       [tr(
	  [th("Name"),
	   th("Size"),
	   th("Limit"),
	   th("Used"),
	   th("Rate")]) |
	lists:map(fun(IndexTable) -> index_tables_table(IndexTable) end,
		  IndexTables)])].

index_tables_table(IndexTable) ->
    #index_table{name=Name,size=Size,limit=Limit,used=Used,rate=Rate} = 
	IndexTable,
    tr(
      [td(Name),
       td("ALIGN=right",Size),
       td("ALIGN=right",Limit),
       td("ALIGN=right",Used),
       td("ALIGN=right",Rate)]).

%%%-----------------------------------------------------------------
%%% Internal library
header(Body) ->
    header("","",Body).
header(Title,Body) ->
    header(Title,"",Body).
header(Title,JavaScript,Body) ->
    ["Pragma:no-cache\r\n",
     "Content-type: text/html\r\n\r\n",
     html_header(Title,JavaScript,Body)].

html_header(Title,JavaScript,Body) ->    
    ["<HTML>\n",
     "<HEAD>\n",
     "<TITLE>", Title,  "</TITLE>\n",
     JavaScript,
     "</HEAD>\n",
     Body,
     "</HTML>"].

body(Text) ->
    ["<BODY BGCOLOR=\"#FFFFFF\">\n",
     Text,
     "<\BODY>\n"].

frameset(Args,Frames) ->
    ["<FRAMESET ",Args,">\n", Frames, "\n</FRAMESET>\n"].
frame(Args) ->
    ["<FRAME ",Args, ">\n"].

table(Args,Text) ->
    ["<TABLE ", Args, ">\n", Text, "\n</TABLE>\n"].
tr(Text) ->
    ["<TR>\n", Text, "\n</TR>\n"].
tr(Args,Text) ->
    ["<TR ", Args, ">\n", Text, "\n</TR>\n"].
th(Text) ->
    ["<TH>", Text, "</TH>"].
th(Args,Text) ->
    ["<TH ", Args, ">\n", Text, "\n</TH>\n"].
td(Text) ->
    ["<TD>", Text, "</TD>"].
td(Args,Text) ->
    ["<TD ", Args, ">", Text, "</TD>"].

b(Text) ->
    ["<B>",Text,"</B>"].
em(Text) ->    
    ["<EM>",Text,"</EM>\n"].
pre(Text) ->
    ["<PRE>",Text,"</PRE>"].
href(Link,Text) ->
    ["<A HREF=\"",Link,"\">",Text,"</A>"].
href(Args,Link,Text) ->
    ["<A HREF=\"",Link,"\" ",Args,">",Text,"</A>"].
img("") ->
    "";
img(Picture) ->
    ["<IMG SRC=\"", Picture, "\" BORDER=0>"].
form(Args,Text) ->
    ["<FORM ",Args,">\n",Text,"\n</FORM>\n"].
input(Args) ->
    ["<INPUT ", Args, ">\n"].
h1(Text) ->
    ["<H1>",Text,"</H1>\n"].
h2(Text) ->
    ["<H2>",Text,"</H2>\n"].
font(Args,Text) ->
    ["<FONT ",Args,">\n",Text,"\n</FONT>\n"].
p(Text) ->    
    ["<P>",Text,"</P>\n"].
p(Args, Text) ->    
    ["<P ", Args, ">",Text,"</P>\n"].
br() ->
    "<BR>\n".


%% In all the following, "<" is changed to "&lt;" and ">" is changed to "&gt;"
href_proc_port(Text) ->
    href_proc_port(Text,[]).
href_proc_port([$#,$R,$e,$f,$<|T],Acc) ->
    %% No links to refs
    href_proc_port(T,[$;,$t,$l,$&,$f,$e,$R,$#|Acc]);
href_proc_port([$#,$F,$u,$n,$<|T],Acc) ->
    %% No links to funs
    href_proc_port(T,[$;,$t,$l,$&,$n,$u,$F,$#|Acc]);
href_proc_port([$#,$P,$o,$r,$t,$<|T],Acc) ->
    {[$#|Port]=HashPort,Rest} = to_gt(T,[$;,$t,$l,$&,$t,$r,$o,$P,$#]),
    href_proc_port(Rest,[href("TARGET=\"main\"",
			      ["./ports?port=",Port],HashPort)|Acc]);
href_proc_port([$<,$<|T],Acc) ->
    %% No links to binaries
    href_proc_port(T,[$;,$t,$l,$&,$;,$t,$l,$&|Acc]);
href_proc_port([$<,C|T],Acc) when $0 =< C, C =< $9 ->
    %% Pid
    {Pid,Rest} = to_gt(T,[C,$;,$t,$l,$&]),
    href_proc_port(Rest,[href("TARGET=\"main\"",
			      ["./proc_details?pid=",Pid],Pid)|Acc]);
href_proc_port([$",$#,$C,$D,$V,$B,$i,$n,$<|T],Acc) ->
    %% Binary written by crashdump_viewer:parse_heap_term(...)
    {SizeAndPos,[$"|Rest]} = split($>,T),
    {Size,Pos} = split($,,SizeAndPos),
    href_proc_port(Rest,[href("TARGET=\"expanded\"",
			      ["./expand_binary?pos=",Pos],
			      ["&lt;&lt; ",Size," bytes &gt;&gt;"]) | Acc]);
href_proc_port([$",$#,$C,$D,$V,$P,$o,$r,$t,$<|T],Acc) ->
    %% Port written by crashdump_viewer:parse_term(...)
    {[$#|Port]=HashPort,[$"|Rest]} = to_gt(T,[$;,$t,$l,$&,$t,$r,$o,$P,$#]),
    href_proc_port(Rest,[href("TARGET=\"main\"",
			      ["./ports?port=",Port],HashPort)|Acc]);
href_proc_port([$",$#,$C,$D,$V,$P,$i,$d,$<|T],Acc) ->
    %% Pid written by crashdump_viewer:parse_term(...)
    {Pid,[$"|Rest]} = to_gt(T,[$;,$t,$l,$&]),
    href_proc_port(Rest,[href("TARGET=\"main\"",
			      ["./proc_details?pid=",Pid],Pid)|Acc]);
href_proc_port([$',$#,$C,$D,$V,$I,$n,$c,$o,$m,$p,$l,$e,$t,$e,$H,$e,$a,$p,$'|T],
	       Acc)->
    %% The heap is incomplete! Written by crashdump_viewer:deref_pts(...)
    IH = lists:reverse(
	   lists:flatten(
	     "<FONT COLOR=\"#FF0000\">...(Incomplete Heap)</FONT>")),
    href_proc_port(T,IH++Acc);
href_proc_port([$',$#,$C,$D,$V,$T,$r,$u,$n,$c,$a,$t,$e,$d,$B,$i,$n,$a,$r,$y,$'
		|T], Acc)->
    %% A binary which is truncated! Written by 
    %% crashdump_viewer:parse_heap_term(...)
    IH = lists:reverse(
	   lists:flatten(
	     "<FONT COLOR=\"#FF0000\">&lt;&lt;...(Truncated Binary)&gt;&gt;"
	     "</FONT>")),
    href_proc_port(T,IH++Acc);
href_proc_port([$',$#,$C,$D,$V,$N,$o,$n,$e,$x,$i,$s,$t,$i,$n,$g,$B,$i,$n,$a,$r,
		$y,$'|T], Acc)->
    %% A binary which could not be found in the dump! Written by 
    %% crashdump_viewer:parse_heap_term(...)
    IH = lists:reverse(
	   lists:flatten(
	     "<FONT COLOR=\"#FF0000\">&lt;&lt;...(Nonexisting Binary)&gt;&gt;"
	     "</FONT>")),
    href_proc_port(T,IH++Acc);
href_proc_port([$<|T],Acc) ->
    href_proc_port(T,[$;,$t,$l,$&|Acc]);
href_proc_port([$>|T],Acc) ->
    href_proc_port(T,[$;,$t,$g,$&|Acc]);
href_proc_port([H|T],Acc) ->
    href_proc_port(T,[H|Acc]);
href_proc_port([],Acc) ->
    lists:reverse(Acc).

to_gt(Str,Acc) ->
    {Match,Rest} = to_gt_noreverse(Str,Acc),
    {lists:reverse(Match),Rest}.
to_gt_noreverse([$>|T],Acc) ->
    {[$;,$t,$g,$&|Acc],T};
to_gt_noreverse([H|T],Acc) ->
    to_gt_noreverse(T,[H|Acc]);
to_gt_noreverse([],Acc) ->
    {Acc,[]}.

split(Char,Str) ->
    split(Char,Str,[]).
split(Char,[Char|Str],Acc) -> % match Char
    {lists:reverse(Acc),Str};
split(Char,[H|T],Acc) ->
    split(Char,T,[H|Acc]).


warn([]) ->
    [];
warn(Warning) ->
    font("COLOR=\"#FF0000\"",p([Warning,br(),br()])).

heading(Heading,HelpMarker) ->
    [font("SIZE=+2",b(Heading)),?space,?space,help(HelpMarker)].

help(HelpMarker) ->
    [href("TARGET=doc",
	  ["/crashdump_doc/crashdump_help.html#",HelpMarker],
	  "Help"),
     br(),br()].

%%%-----------------------------------------------------------------
%%% This function pretty formats a string which contains erlang
%%% terms (e.g. the message queue).
%%% In all the following, "<" is changed to "&lt;" and ">" is changed to "&gt;"
pretty_format(In) ->
    case catch scan(In,[],initial,[]) of
	{'EXIT',_Reason} ->
	    %% Probably a truncated file, so the erlang term is not complete
	    [font("COLOR=\"#FF0000\"","(This term might be truncated)"),
	     href_proc_port(lists:flatten(In))];
	{[R],_,Insrt} ->
	    InsrtString = lists:flatten(io_lib:format("~p",[R])),
	    lists:flatten(replace_insrt(lists:reverse(InsrtString),Insrt,[]))
    end.

%% Finish term
scan(In,Acc,list,Insrt) when hd(In)==$] ->
    {lists:reverse(Acc),tl(In),Insrt};    
scan(In,Acc,tuple,Insrt) when hd(In)==$} ->
    {list_to_tuple(lists:reverse(Acc)),tl(In),Insrt};    
scan(In,Acc,atom,Insrt) when In==[];hd(In)==$,;hd(In)==$];hd(In)==$} ->
    {list_to_atom(lists:reverse(Acc)),In,Insrt};
scan(In,Acc,float,Insrt) when In==[];hd(In)==$,;hd(In)==$];hd(In)==$} ->
    {list_to_float(lists:reverse(Acc)),In,Insrt};
scan(In,Acc,integer,Insrt) when In==[];hd(In)==$,;hd(In)==$];hd(In)==$} ->
     {list_to_integer(lists:reverse(Acc)),In,Insrt};
scan([$"|In],Acc,string,Insrt) when In==[];hd(In)==$,;hd(In)==$];hd(In)==$} -> 
    {lists:reverse(Acc),In,Insrt};
scan([$>|In],Acc,special,Insrt) when In==[];hd(In)==$,;hd(In)==$];hd(In)==$} ->
    %% pid, ref, port, fun
    {lists:reverse([$;,$t,$g,$&|Acc]),In,Insrt};
scan([$}|In],Acc,special,Insrt) when In==[];hd(In)==$,;hd(In)==$];hd(In)==$} ->
    %% bignum integer, e.g. #integer(2) = {2452,4324}
    {lists:reverse([$}|Acc]),In,Insrt};
scan([$,|In],Acc,Cur,Insrt) when Cur/=string,Cur/=special ->
    scan(In,Acc,Cur,Insrt);

%% In the middle of an atom
scan([$'|In],Acc,Cur,Insrt) when Cur==atom ->
    %% all $' are removed. They are added again by list_to_atom,
    %% so if we don't remove them we will get two of them.
    scan(In,Acc,Cur,Insrt);

%% A $. in the middle of an integer - turn to float
scan([C|T],Acc,integer,Insrt) when C==$. ->
    scan(T,[C|Acc],float,Insrt);

%% In the middle of an atom, integer, float or string
scan([$<|T],Acc,Cur,Insrt) when Cur==atom;Cur==string;Cur==special ->
    scan(T,[$;,$t,$l,$&|Acc],Cur,Insrt);
scan([$>|T],Acc,Cur,Insrt) when Cur==atom;Cur==string ->
    scan(T,[$;,$t,$g,$&|Acc],Cur,Insrt);
scan([C|T],Acc,Cur,Insrt) when Cur==atom;Cur==integer;Cur==float;Cur==string;Cur==special ->
    scan(T,[C|Acc],Cur,Insrt);

%% Start list
scan([$[|T],Acc,Cur,Insrt0) ->
    {L,Rest,Insrt} = scan(T,[],list,Insrt0),
    scan(Rest,[L|Acc],Cur,Insrt);

%% Star tuple
scan([${|T],Acc,Cur,Insrt0) ->
    {Tuple,Rest,Insrt} = scan(T,[],tuple,Insrt0),
    scan(Rest,[Tuple|Acc],Cur,Insrt);

%% Star string
scan([$"|T],Acc,Cur,Insrt0) ->
    {String,Rest,Insrt} = scan(T,[],string,Insrt0),
    scan(Rest,[String|Acc],Cur,Insrt);

%% Start atom
scan([$'|T],Acc,Cur,Insrt0) ->
    %% all $' are removed. They are added again by list_to_atom,
    %% so if we don't remove them we will get two of them.
    {Atom,Rest,Insrt} = scan(T,[],atom,Insrt0),
    scan(Rest,[Atom|Acc],Cur,Insrt);
scan([C|T],Acc,Cur,Insrt0) when C>=$A,C=<$Z;C>=$a,C=<$z;C==$'->
    {Atom,Rest,Insrt} = scan(T,[C],atom,Insrt0),
    scan(Rest,[Atom|Acc],Cur,Insrt);

%% Start integer or float
scan([C|T],Acc,Cur,Insrt0) when C>=$0,C=<$9;C==$- ->
    {Num,Rest,Insrt} = scan(T,[C],integer,Insrt0), % can later change to float
    scan(Rest,[Num|Acc],Cur,Insrt);

%% Start Pid/Port/Ref/Fun/Binary
scan([$<|T],Acc,Cur,Insrt0) ->
    {Special,Rest,Insrt} = scan(T,[$;,$t,$l,$&],special,Insrt0),
    scan(Rest,['$insrt'|Acc],Cur,[Special|Insrt]);
scan([$#|T],Acc,Cur,Insrt0) ->
    {Special,Rest,Insrt} = scan(T,[$#],special,Insrt0),
    scan(Rest,['$insrt'|Acc],Cur,[Special|Insrt]);


%% done
scan([],Acc,initial,Insrt) ->
    {Acc,[],Insrt}.


replace_insrt("'trsni$'"++Rest,[H|T],Acc) -> % the list is reversed here!
    Special = 
	case H of
	    "&lt;&lt;" ++ _Binary -> 
		H;
	    "&lt;" ++ _Pid -> 
		href("TARGET=\"main\"",["./proc_details?pid=",H],H);
	    "#Port&lt;" ++ Port -> 
		href("TARGET=\"main\"",["./ports?port=","Port&lt;"++Port],H);
	    "#" ++ _other -> 
		H
	end,
    replace_insrt(Rest,T,[Special|Acc]);
replace_insrt([H|T],Insrt,Acc) ->
    replace_insrt(T,Insrt,[H|Acc]);
replace_insrt([],[],Acc) ->
    Acc.
