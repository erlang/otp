%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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
-module(cdv_mem_cb).

-export([get_info/0]).

-include("crashdump_viewer.hrl").
-include_lib("wx/include/wx.hrl").

get_info() ->
    observer_lib:report_progress({ok,"Processing memory info"}),
    MemInfo = get_mem_info(),
    observer_lib:report_progress({ok,33}),
    {AllocInfo,AllocTW} = get_alloc_info(),
    observer_lib:report_progress({ok,66}),
    AreaInfo = get_area_info(),
    observer_lib:report_progress({ok,100}),
    [{"Memory",cdv_info_wx,MemInfo}
     | [{Title,cdv_table_wx,{Cols,Data,AllocTW}} ||
	   {Title,Cols,Data} <- AllocInfo]] ++
	[{"Allocated Areas",cdv_table_wx,AreaInfo}].


%%%-----------------------------------------------------------------
%%% Memory page
get_mem_info() ->
    {ok,Info,TW} = crashdump_viewer:memory(),
    {[{"Memory Information",gen_mem_info_fields(Info)}],Info,TW}.

gen_mem_info_fields([{Key,_}|T]) ->
    [{upper(atom_to_list(Key)),{bytes,Key}}|gen_mem_info_fields(T)];
gen_mem_info_fields([]) ->
    [].

upper(Key) ->
    lists:join(" ", [string:titlecase(Word) || Word <- string:split(Key, "_", all)]).

%%%-----------------------------------------------------------------
%%% Allocated areas page
get_area_info() ->
    {ok,Info0,TW} = crashdump_viewer:allocated_areas(),
    Info = [tuple_to_list(R) || R <- Info0],
    {area_columns(),Info,TW}.

area_columns() ->
    [{"",                 ?wxLIST_FORMAT_LEFT,  150},
     {"Allocated (bytes)",?wxLIST_FORMAT_RIGHT, 150},
     {"Used (bytes)",     ?wxLIST_FORMAT_RIGHT, 150}].

%%%-----------------------------------------------------------------
%%% Allocator page
get_alloc_info() ->
    {ok,Info,TW} = crashdump_viewer:allocator_info(),
    {fix_alloc(Info),TW}.

fix_alloc([{Title,Columns,Data}|Tables]) ->
    [{Title,alloc_columns(Columns),
     [[Key|Values] || {Key,Values} <- Data]} |
     fix_alloc(Tables)];
fix_alloc([{Title,[{_,V}|_]=Data}|Tables]) ->
    fix_alloc([{Title,lists:duplicate(length(V),[]),Data}|Tables]);
fix_alloc([{"",[]}|Tables]) -> % no name and no data, probably truncated dump
    fix_alloc(Tables);
fix_alloc([{Title,[]=Data}|Tables]) -> % no data, probably truncated dump
    fix_alloc([{Title,[],Data}|Tables]);
fix_alloc([]) ->
    [].

alloc_columns(Columns) ->
    [{"",   ?wxLIST_FORMAT_LEFT,  180} |
     [{Column, ?wxLIST_FORMAT_RIGHT, 140} || Column <- Columns]].
