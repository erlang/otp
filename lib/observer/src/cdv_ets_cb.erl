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
-module(cdv_ets_cb).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Defines
-define(COL_ID,    0).
-define(COL_NAME,  ?COL_ID+1).
-define(COL_SLOT,  ?COL_NAME+1).
-define(COL_OWNER, ?COL_SLOT+1).
-define(COL_BUCK,  ?COL_OWNER+1).
-define(COL_OBJ,   ?COL_BUCK+1).
-define(COL_MEM,   ?COL_OBJ+1).
-define(COL_TYPE,  ?COL_MEM+1).

%% Callbacks for cdv_virtual_list_wx
col_to_elem(id) -> col_to_elem(?COL_ID);
col_to_elem(?COL_ID)    -> #ets_table.id;
col_to_elem(?COL_NAME)  -> #ets_table.name;
col_to_elem(?COL_SLOT)  -> #ets_table.slot;
col_to_elem(?COL_OWNER) -> #ets_table.pid;
col_to_elem(?COL_TYPE)  -> #ets_table.type;
col_to_elem(?COL_BUCK)  -> #ets_table.buckets;
col_to_elem(?COL_OBJ)   -> #ets_table.size;
col_to_elem(?COL_MEM)   -> #ets_table.memory.

col_spec() ->
    [{"Id",      ?wxLIST_FORMAT_LEFT,   200},
     {"Name",    ?wxLIST_FORMAT_LEFT,   200},
     {"Slot",    ?wxLIST_FORMAT_RIGHT,  50},
     {"Owner",   ?wxLIST_FORMAT_CENTRE, 90},
     {"Buckets", ?wxLIST_FORMAT_RIGHT,  50},
     {"Objects", ?wxLIST_FORMAT_RIGHT,  50},
     {"Memory",  ?wxLIST_FORMAT_RIGHT,  80},
     {"Type",    ?wxLIST_FORMAT_LEFT,   50}
    ].

get_info(Owner) ->
    {ok,Info,TW} = crashdump_viewer:ets_tables(Owner),
    {Info,TW}.

get_detail_cols(all) ->
    {[?COL_OWNER],false};
get_detail_cols(_) ->
    {[],false}.
