%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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
-module(cdv_proc_cb).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1,
	 get_details/2,
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

%% Callbacks for cdv_virtual_list_wx
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
     {"Reds", ?wxLIST_FORMAT_RIGHT, 80},
     {"Memory", ?wxLIST_FORMAT_RIGHT, 80},
     {"MsgQ",  ?wxLIST_FORMAT_RIGHT, 50}].

get_info(_) ->
    {ok,Info,TW} = crashdump_viewer:processes(),
    {Info,TW}.

get_detail_cols(_) ->
    {[{process, ?COL_ID}],true}.

%% Callbacks for cdv_detail_wx
get_details(Id, _) ->
    case crashdump_viewer:proc_details(Id) of
	{ok,Info,TW} ->
	    %% The following table is used by observer_html_lib
	    %% for storing expanded terms and it is read by
	    %% cdv_html_wx when a link to an expandable term is clicked.
	    Tab = ets:new(cdv_expand,[set,public]),
	    Proplist0 =
		crashdump_viewer:to_proplist(record_info(fields,proc),Info),
	    Proplist = [{expand_table,Tab}|Proplist0],
	    Title = io_lib:format("~ts (~s)",[Info#proc.name, Id]),
	    {ok,{Title,Proplist,TW}};
	{error,{other_node,NodeId}} ->
	    Info = "The process you are searching for was residing on "
		"a remote node. No process information is available. "
		"Show information about the remote node?",
	    Fun = fun() -> cdv_virtual_list_wx:start_detail_win(NodeId, port) end,
	    {yes_no, Info, Fun};
	{error,not_found} ->
	    Info = "The process you are searching for could not be found.",
	    {info,Info}
    end.

detail_pages() ->
    [{"General Information",   fun init_gen_page/2},
     {"Messages",   fun init_message_page/2},
     {"Dictionary", fun init_dict_page/2},
     {"Stack Dump", fun init_stack_page/2},
     {"ETS tables", fun init_ets_page/2},
     {"Timers",     fun init_timer_page/2}].

init_gen_page(Parent, Info) ->
    Fields = info_fields(),
    cdv_info_wx:start_link(Parent,{Fields,Info,[]}).

init_message_page(Parent, Info) ->
    init_memory_page(Parent, Info, msg_q, "MsgQueue").

init_dict_page(Parent, Info) ->
    init_memory_page(Parent, Info, dict, "Dictionary").

init_stack_page(Parent, Info) ->
    init_memory_page(Parent, Info, stack_dump, "StackDump").

init_memory_page(Parent, Info0, Tag, Heading) ->
    Info = proplists:get_value(Tag,Info0),
    Tab = proplists:get_value(expand_table,Info0),
    Html = observer_html_lib:expandable_term(Heading,Info,Tab, observer_lib:colors(Parent)),
    cdv_html_wx:start_link(Parent,{expand,Html,Tab}).

init_ets_page(Parent, Info) ->
    Pid = proplists:get_value(pid,Info),
    cdv_virtual_list_wx:start_link(Parent, cdv_ets_cb, Pid).

init_timer_page(Parent, Info) ->
    Pid = proplists:get_value(pid,Info),
    cdv_virtual_list_wx:start_link(Parent, cdv_timer_cb, Pid).

%%%-----------------------------------------------------------------
%%% Internal
info_fields() ->
    [{"Overview",
      [{"Initial Call",     init_func},
       {dynamic,            current_func},
       {"Registered Name",  name},
       {"Status",           state},
       {"Internal State",   int_state},
       {"Started",          start_time},
       {"Parent",           {click,parent}},
       {"Message Queue Len",msg_q_len},
       {"Run queue",        run_queue},
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
       {"Binary vheap",     bin_vheap},
       {"Old Binary vheap", old_bin_vheap},
       {"Binary vheap unused",  bin_vheap_unused},
       {"Old Binary vheap unused", old_bin_vheap_unused},
       {"Number of Heap Fragements", num_heap_frag},
       {"Heap Fragment Data",heap_frag_data},
       {"New Heap Start",   new_heap_start},
       {"New Heap Top",     new_heap_top},
       {"Stack Top",        stack_top},
       {"Stack End",        stack_end},
       {"Old Heap Start",   old_heap_start},
       {"Old Heap Top",     old_heap_top},
       {"Old Heap End",     old_heap_end}]}].
