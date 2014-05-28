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
    {[?COL_OWNER],false};
get_detail_cols(_) ->
    {[],false}.
