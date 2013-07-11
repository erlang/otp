%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
-module(cdv_proc_wx).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1,
	 get_details/1,
	 detail_pages/0]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Columns
-define(COL_ID,  0).
-define(COL_NAME, ?COL_ID+1).
-define(COL_STATE,?COL_NAME+1).
-define(COL_REDS, ?COL_STATE+1).
-define(COL_MEM,  ?COL_REDS+1).
-define(COL_MSG,  ?COL_MEM+1).

%% Callbacks for cdv_virtual_list
col_to_elem(id) -> col_to_elem(?COL_ID);
col_to_elem(?COL_ID)  -> #proc.pid;
col_to_elem(?COL_NAME) -> #proc.name;
col_to_elem(?COL_STATE) -> #proc.state;
col_to_elem(?COL_MEM)  -> #proc.memory;
col_to_elem(?COL_REDS) -> #proc.reds;
col_to_elem(?COL_MSG)  -> #proc.msg_q_len.

col_spec() ->
    [{"Pid", ?wxLIST_FORMAT_CENTRE,  120},
     {"Name or Initial Func", ?wxLIST_FORMAT_LEFT, 250},
     {"State", ?wxLIST_FORMAT_LEFT, 100},
     {"Reds", ?wxLIST_FORMAT_RIGHT, 100},
     {"Memory", ?wxLIST_FORMAT_RIGHT, 100},
     {"MsgQ",  ?wxLIST_FORMAT_RIGHT, 100}].

get_info(_) ->
    {ok,Info,TW} = crashdump_viewer:processes(),
    {Info,TW}.

get_detail_cols(_) ->
    {[?COL_ID],true}.

%% Callbacks for cdv_detail_win
get_details(Id) ->
    case crashdump_viewer:proc_details(Id) of
	{ok,Info,TW} ->
	    Proplist =
		crashdump_viewer:to_proplist(record_info(fields,proc),Info),
	    Title = io_lib:format("~s (~p)",[Info#proc.name, Id]),
	    {ok,{Title,Proplist,TW}};
	{error,{other_node,NodeId}} ->
	    Info = "The process you are searching for was residing on "
		"a remote node. No process information is available. "
		"Show information about the remote node?",
	    {yes_no, Info, fun()->cdv_virtual_list:start_detail_win(NodeId) end};
	{error,not_found} ->
	    Info = "The process you are searching for could not be found.",
	    {info,Info}
    end.

detail_pages() ->
    [{simple, "General Information",   fun init_gen_page/3},
     {simple, "Messages",   fun init_message_page/3},
     {simple, "Dictionary", fun init_dict_page/3},
     {simple, "Stack Dump", fun init_stack_page/3},
     {list,   "ETS tables", fun init_ets_page/3},
     {list,   "Timers",     fun init_timer_page/3}].

init_gen_page(Parent, _Pid, Info) ->
    Fields = info_fields(),
    cdv_detail_win:init_detail_page(Parent, Fields, Info).

init_message_page(Parent, Pid, _Info) ->
    init_memory_page(Parent, Pid, "MsgQueue").

init_dict_page(Parent, Pid, _Info) ->
    init_memory_page(Parent, Pid, "Dictionary").

init_stack_page(Parent, Pid, _Info) ->
    init_memory_page(Parent, Pid, "StackDump").

init_memory_page(Parent, Pid, What) ->
    Html =
	case crashdump_viewer:expand_memory(Pid,What) of
	    {ok,Memory} ->
		crashdump_viewer_html:expanded_memory(What,Memory);
	    {error,Reason} ->
		crashdump_viewer_html:warning(Reason)
	end,
    observer_lib:html_window(Parent,Html).

init_ets_page(Parent, Pid, _Info) ->
    cdv_virtual_list:start_link(Parent, cdv_ets_wx, Pid).

init_timer_page(Parent, Pid, _Info) ->
    cdv_virtual_list:start_link(Parent, cdv_timer_wx, Pid).

%%%-----------------------------------------------------------------
%%% Internal
info_fields() ->
    [{"Overview",
      [{"Initial Call",     init_func},
       {dynamic,            current_func},
       {"Registered Name",  name},
       {"Status",           state},
       {"Started",          start_time},
       {"Parent",           {click,parent}},
       {"Message Queue Len",msg_q_len},
       {"Reductions",       reds},
       {"Program counter",  prog_count},
       {"Continuation pointer",cp},
       {"Arity",arity}]},
     {scroll_boxes,
      [{"Last Calls",1,{plain,last_calls}}]},
     {scroll_boxes,
      [{"Links",1,{click,links}},
       {"Monitors",2,{click,monitors}},
       {"Monitored By",2,{click,mon_by}}]},
     {"Memory and Garbage Collection",
      [{"Memory",           memory},
       {"Stack and Heap",   stack_heap},
       {"Old Heap",         old_heap},
       {"Heap Unused",      heap_unused},
       {"Old Heap Unused",  old_heap_unused},
       {"Number of Heap Fragements", num_heap_frag},
       {"Heap Fragment Data",heap_frag_data},
       {"New Heap Start",   new_heap_start},
       {"New Heap Top",     new_heap_top},
       {"Stack Top",        stack_top},
       {"Stack End",        stack_end},
       {"Old Heap Start",   old_heap_start},
       {"Old Heap Top",     old_heap_top},
       {"Old Heap End",     old_heap_end}]}].
