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
-module(cdv_port_wx).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1,
	 get_details/1,
	 detail_pages/0,
	 format/1]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Columns
-define(COL_ID,  0).
-define(COL_CONN, ?COL_ID+1).
-define(COL_NAME, ?COL_CONN+1).
-define(COL_CTRL, ?COL_NAME+1).
-define(COL_SLOT, ?COL_CTRL+1).



%% Callbacks for cdv_virtual_list
col_to_elem(id) -> col_to_elem(?COL_ID);
col_to_elem(?COL_ID)  -> #port.id;
col_to_elem(?COL_CONN) -> #port.connected;
col_to_elem(?COL_NAME)  -> #port.name;
col_to_elem(?COL_CTRL) -> #port.controls;
col_to_elem(?COL_SLOT) -> #port.slot.

col_spec() ->
    [{"Id", ?wxLIST_FORMAT_LEFT,  120},
     {"Connected", ?wxLIST_FORMAT_LEFT, 120},
     {"Name", ?wxLIST_FORMAT_LEFT, 200},
     {"Controls", ?wxLIST_FORMAT_LEFT, 250},
     {"Slot", ?wxLIST_FORMAT_RIGHT, 50}].

get_info(_) ->
    {ok,Info,TW} = crashdump_viewer:ports(),
    {Info,TW}.

get_detail_cols(_) ->
    {[?COL_ID,?COL_CONN],true}.

%% Callbacks for cdv_detail_win
get_details(Id) ->
    case crashdump_viewer:port(Id) of
	{ok,Info,TW} ->
	    Proplist =
		crashdump_viewer:to_proplist(record_info(fields,port),Info),
	    {ok,{Id,Proplist,TW}};
	{error,{other_node,NodeId}} ->
	    Info = "The port you are searching for was residing on "
		"a remote node. No port information is available. "
		"Show information about the remote node?",
	    {yes_no, Info, fun()->cdv_virtual_list:start_detail_win(NodeId) end};
	{error,not_found} ->
	    Info = "The port you are searching for could not be found.",
	    {info,Info}
    end.

detail_pages() ->
    [{simple, "General Information",   fun init_gen_page/3}].

init_gen_page(Parent, _Id, Info) ->
    Fields = info_fields(),
    cdv_detail_win:init_detail_page(Parent, Fields, Info).

format({I1,I2}) ->
    "#Port<"++integer_to_list(I1) ++ "." ++ integer_to_list(I2) ++ ">";
format(D) ->
    D.


%%%-----------------------------------------------------------------
%%% Internal
info_fields() ->
    [{"Overview",
      [{"Name",             name},
       {"Connected",        connected},
       {"Slot",             slot},
       {"Controls",         controls}]},
    {scroll_boxes,
     [{"Links",1,{click,links}},
       {"Monitors",1,{click,monitors}}]}].
