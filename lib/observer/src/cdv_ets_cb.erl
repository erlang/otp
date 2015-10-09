%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
-module(cdv_ets_cb).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_details/2,
	 get_detail_cols/1,
	 detail_pages/0
	]).

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
col_to_elem(?COL_TYPE)  -> #ets_table.data_type;
col_to_elem(?COL_BUCK)  -> #ets_table.buckets;
col_to_elem(?COL_OBJ)   -> #ets_table.size;
col_to_elem(?COL_MEM)   -> #ets_table.memory.

col_spec() ->
    [{"Id",      ?wxLIST_FORMAT_LEFT,   200},
     {"Name",    ?wxLIST_FORMAT_LEFT,   200},
     {"Slot",    ?wxLIST_FORMAT_RIGHT,  50},
     {"Owner",   ?wxLIST_FORMAT_CENTRE, 120},
     {"Objects", ?wxLIST_FORMAT_RIGHT,  80},
     {"Memory",  ?wxLIST_FORMAT_RIGHT,  80}
%     {"Type",    ?wxLIST_FORMAT_LEFT,   50}
    ].

get_info(Owner) ->
    {ok,Info,TW} = crashdump_viewer:ets_tables(Owner),
    {Info,TW}.

%% Callbacks for cdv_detail_wx
get_details(_Id, not_found) ->
    Info = "The table you are searching for could not be found.",
    {info,Info};
get_details(Id, Data) ->
    Proplist = crashdump_viewer:to_proplist(record_info(fields,ets_table),Data),
    {ok,{"Table:" ++ Id,Proplist,""}}.

get_detail_cols(all) ->
    {[{ets, ?COL_ID}, {process, ?COL_OWNER}],true};
get_detail_cols(_W) ->
    {[],true}.


%%%%%%%%%%%%%%%%%%%%%%%%

detail_pages() ->
    [{"Table Information",   fun init_gen_page/2}].

init_gen_page(Parent, Info0) ->
    Fields = info_fields(),
    Details = proplists:get_value(details, Info0),
    Info = if is_map(Details) -> Info0 ++ maps:to_list(Details);
	      true -> Info0
	   end,
    cdv_info_wx:start_link(Parent,{Fields,Info,[]}).

%%% Internal
info_fields() ->
    [{"Overview",
      [{"Id",             id},
       {"Name",           name},
       {"Slot",           slot},
       {"Owner",          owner},
       {"Data Structure", data_type}
      ]},
     {"Settings",
      [{"Type",           type},
       {"Protection",     protection},
       {"Compressed",     compressed},
       {"Fixed",          fixed},
       {"Lock write concurrency", write_c},
       {"Lock read concurrency", read_c}
      ]},
     {"Memory Usage",
      [{"Buckets",        buckets},
       {"Size",           size},
       {"Memory",         memory},
       {"Min Chain Length",  chain_min},
       {"Avg Chain Length",  chain_avg},
       {"Max Chain Length",  chain_max},
       {"Chain Length Std Dev",  chain_stddev},
       {"Chain Length Expected Std Dev",  chain_exp_stddev}
      ]}
    ].
