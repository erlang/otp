%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-module(cdv_fun_cb).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Defines
-define(COL_MOD,    0).
-define(COL_UNIQ,  ?COL_MOD+1).
-define(COL_INDEX,  ?COL_UNIQ+1).
-define(COL_ADDR, ?COL_INDEX+1).
-define(COL_NADDR,  ?COL_ADDR+1).
-define(COL_REFC,  ?COL_NADDR+1).

%% Callbacks for cdv_virtual_list_wx
col_to_elem(id) -> col_to_elem(?COL_MOD);
col_to_elem(?COL_MOD)   -> #fu.module;
col_to_elem(?COL_UNIQ)  -> #fu.uniq;
col_to_elem(?COL_INDEX) -> #fu.index;
col_to_elem(?COL_ADDR)  -> #fu.address;
col_to_elem(?COL_NADDR) -> #fu.native_address;
col_to_elem(?COL_REFC)  -> #fu.refc.

col_spec() ->
    [{"Module",         ?wxLIST_FORMAT_LEFT,   200},
     {"Uniq",           ?wxLIST_FORMAT_RIGHT,  100},
     {"Index",          ?wxLIST_FORMAT_RIGHT,  50},
     {"Address",        ?wxLIST_FORMAT_LEFT,   120},
     {"Native Address", ?wxLIST_FORMAT_LEFT,   120},
     {"Refc",           ?wxLIST_FORMAT_RIGHT,  50}].

get_info(_) ->
    {ok,Info,TW} = crashdump_viewer:funs(),
    {Info,TW}.

get_detail_cols(_) ->
    {[{module, ?COL_MOD}],false}.
