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
-module(cdv_timer_cb).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Defines
-define(COL_OWNER, 0).
-define(COL_NAME,   ?COL_OWNER+1).
-define(COL_MSG,   ?COL_NAME+1).
-define(COL_TIME,  ?COL_MSG+1).

%% Callbacks for cdv_virtual_list_wx
col_to_elem(id) -> col_to_elem(?COL_OWNER);
col_to_elem(?COL_OWNER) -> #timer.pid;
col_to_elem(?COL_NAME) -> #timer.name;
col_to_elem(?COL_MSG)   -> #timer.msg;
col_to_elem(?COL_TIME)  -> #timer.time.

col_spec() ->
    [{"Owner",      ?wxLIST_FORMAT_LEFT,   110},
     {"Owner name", ?wxLIST_FORMAT_LEFT,   150},
     {"Message",    ?wxLIST_FORMAT_LEFT,   300},
     {"Time left (ms)",  ?wxLIST_FORMAT_RIGHT,  80}].

get_info(Owner) ->
    {ok,Info,TW} = crashdump_viewer:timers(Owner),
    {Info,TW}.

get_detail_cols(all) ->
    {[{process, ?COL_OWNER}],false};
get_detail_cols(_) ->
    {[],false}.
