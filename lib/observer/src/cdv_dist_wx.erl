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
-module(cdv_dist_wx).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1,
	 get_details/1,
	 detail_pages/0]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Columns
-define(COL_NAME,  0).
-define(COL_TYPE, ?COL_NAME+1).
-define(COL_CTRL, ?COL_TYPE+1).
-define(COL_CH,   ?COL_CTRL+1).
-define(COL_CRE,  ?COL_CH+1).

%% Callbacks for cdv_virtual_list
col_to_elem(id) -> col_to_elem(?COL_CH);
col_to_elem(?COL_NAME)  -> #nod.name;
col_to_elem(?COL_CH)    -> #nod.channel;
col_to_elem(?COL_CTRL)  -> #nod.controller;
col_to_elem(?COL_CRE)   -> #nod.creation;
col_to_elem(?COL_TYPE)  -> #nod.conn_type.

col_spec() ->
    [{"Name",            ?wxLIST_FORMAT_LEFT,   300},
     {"Connection type", ?wxLIST_FORMAT_LEFT,   130},
     {"Controller",      ?wxLIST_FORMAT_LEFT,   130},
     {"Channel",         ?wxLIST_FORMAT_RIGHT,  80},
     {"Creation",        ?wxLIST_FORMAT_RIGHT,  80}].

get_info(_) ->
    {ok,Info,TW} = crashdump_viewer:dist_info(),
    {Info,TW}.

get_detail_cols(_) ->
    {[?COL_CH,?COL_CTRL],true}.

%% Callbacks for cdv_detail_win
get_details(Id) ->
    {ok,Info,TW} = crashdump_viewer:node_info(Id),
    Proplist = crashdump_viewer:to_proplist(record_info(fields,nod),Info),
    Title = io_lib:format("~s (~s)",[Info#nod.name,Id]),
    {ok,{Title,Proplist,TW}}.

detail_pages() ->
    [{"General Information",   fun init_gen_page/2}].

init_gen_page(Parent, Info) ->
    Fields = info_fields(),
    cdv_info_page:start_link(Parent,{Fields,Info,[]}).

%%%-----------------------------------------------------------------
%%% Internal
info_fields() ->
    [{"Overview",
      [{"Name",               name},
       {"Type",               conn_type},
       {"Channel",            channel},
       {"Controller",         {click,controller}},
       {"Creation",           creation},
       {"Extra Info",         error}]},
    {scroll_boxes,
     [{"Remote Links",1,{click,remote_links}},
      {"Remote Monitors",1,{click,remote_mon}},
      {"Remote Monitored By",1,{click,remote_mon_by}}]}].
