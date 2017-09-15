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
-module(cdv_dist_cb).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1,
	 get_details/2,
	 detail_pages/0,
	 format/1]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Columns
-define(COL_NAME,  0).
-define(COL_TYPE, ?COL_NAME+1).
-define(COL_CTRL, ?COL_TYPE+1).
-define(COL_CH,   ?COL_CTRL+1).
-define(COL_CRE,  ?COL_CH+1).

%% Callbacks for cdv_virtual_list_wx
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
    {[{node, ?COL_CH},{port,?COL_CTRL}],true}.

%% Callbacks for cdv_detail_wx
get_details(Id, _) ->
    case crashdump_viewer:node_info(Id) of
	{ok,Info,TW} ->
	    Proplist = crashdump_viewer:to_proplist(record_info(fields,nod),Info),
	    Title = io_lib:format("~s (~s)",[Info#nod.name,Id]),
	    {ok,{Title,Proplist,TW}};
	{error,not_found} ->
	    Info = "The node you are searching for could not be found.",
	    {info,Info}
    end.

detail_pages() ->
    [{"General Information",   fun init_gen_page/2}].

init_gen_page(Parent, Info) ->
    Fields = info_fields(),
    cdv_info_wx:start_link(Parent,{Fields,Info,[]}).

format({creations,Creations}) ->
    lists:flatten(lists:join(",",[integer_to_list(C) || C <- Creations]));
format(D) ->
    D.

%%%-----------------------------------------------------------------
%%% Internal
info_fields() ->
    [{"Overview",
      [{"Name",               name},
       {"Type",               conn_type},
       {"Channel",            channel},
       {"Controller",         {click,controller}},
       {"Creation",           {{format,fun format/1},creation}},
       {"Extra Info",         error}]},
    {scroll_boxes,
     [{"Remote Links",1,{click,remote_links}},
      {"Remote Monitors",1,{click,remote_mon}},
      {"Remote Monitored By",1,{click,remote_mon_by}}]}].
