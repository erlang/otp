%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
-module(cdv_int_tab_cb).

-export([get_info/0]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

get_info() ->
    observer_lib:report_progress({ok,"Processing internal tables"}),
    HashInfo = get_hash_info(),
    observer_lib:report_progress({ok,33}),
    IndexInfo = get_index_info(),
    observer_lib:report_progress({ok,66}),
    IntEtsInfo = get_internal_ets_info(),
    observer_lib:report_progress({ok,100}),
    [{"Hash Tables",cdv_table_wx,HashInfo},
     {"Index Tables",cdv_table_wx,IndexInfo},
     {"Internal ETS Tables",cdv_table_wx,IntEtsInfo}].

%%%-----------------------------------------------------------------
%%% Hash tables
get_hash_info() ->
    {ok,Info0,TW} = crashdump_viewer:hash_tables(),
    Columns = hash_columns(),
    Info = [crashdump_viewer:to_value_list(R) || R <- Info0],
    {Columns,Info,TW}.

hash_columns() ->
    [{"Name",   ?wxLIST_FORMAT_LEFT,  150},
     {"Size",   ?wxLIST_FORMAT_RIGHT, 100},
     {"Used",   ?wxLIST_FORMAT_RIGHT, 100},
     {"Objects",?wxLIST_FORMAT_RIGHT, 100},
     {"Depth",  ?wxLIST_FORMAT_RIGHT, 100}].

%%%-----------------------------------------------------------------
%%% Index tables
get_index_info() ->
    {ok,Info0,TW} = crashdump_viewer:index_tables(),
    Columns = index_columns(),
    Info = [crashdump_viewer:to_value_list(R) || R <- Info0],
    {Columns,Info,TW}.

index_columns() ->
    [{"Name",   ?wxLIST_FORMAT_LEFT,  150},
     {"Size",   ?wxLIST_FORMAT_RIGHT, 100},
     {"Limit",  ?wxLIST_FORMAT_RIGHT, 100},
     {"Used",   ?wxLIST_FORMAT_RIGHT, 100},
     {"Rate",   ?wxLIST_FORMAT_RIGHT, 100},
     {"Entries",?wxLIST_FORMAT_RIGHT, 100}].

%%%-----------------------------------------------------------------
%%% Internal ets tables
get_internal_ets_info() ->
    {ok,Info0,TW} = crashdump_viewer:internal_ets_tables(),
    Columns = int_ets_columns(),
    Info = [begin
		[_,_|Data] = crashdump_viewer:to_value_list(R), %skip pid and slot
		[Desc|Data]
	    end || {Desc,R} <- Info0],
    {Columns,Info,TW}.

int_ets_columns() ->
    [{"Description", ?wxLIST_FORMAT_LEFT,  170},
     {"Id",          ?wxLIST_FORMAT_LEFT,  80},
     {"Name",        ?wxLIST_FORMAT_LEFT,  80},
     {"Type",        ?wxLIST_FORMAT_LEFT,  80},
     {"Buckets",     ?wxLIST_FORMAT_RIGHT, 80},
     {"Objects",     ?wxLIST_FORMAT_RIGHT, 80},
     {"Memory",      ?wxLIST_FORMAT_RIGHT, 80}].
