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
-module(cdv_mod_cb).

-export([col_to_elem/1,
	 col_spec/0,
	 get_info/1,
	 get_detail_cols/1,
	 get_details/2,
	 detail_pages/0,
	 format/1]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Defines
-define(COL_ID,    0).
-define(COL_CUR,  ?COL_ID+1).
-define(COL_OLD,  ?COL_CUR+1).

%% Callbacks for cdv_virtual_list_wx
col_to_elem(id) -> col_to_elem(?COL_ID);
col_to_elem(?COL_ID)   -> #loaded_mod.mod;
col_to_elem(?COL_CUR)  -> #loaded_mod.current_size;
col_to_elem(?COL_OLD)  -> #loaded_mod.old_size.

col_spec() ->
    [{"Module",       ?wxLIST_FORMAT_LEFT,   300},
     {"Current size", ?wxLIST_FORMAT_RIGHT,  80},
     {"Old size",     ?wxLIST_FORMAT_RIGHT,  80}].

get_info(_) ->
    {ok,Info,TW} = crashdump_viewer:loaded_modules(),
    {Info,TW}.

get_detail_cols(_) ->
    {[{module, ?COL_ID}],true}.

%% Callbacks for cdv_detail_wx
get_details(Id, _) ->
    {ok,Info,TW} = crashdump_viewer:loaded_mod_details(Id),
    Proplist = crashdump_viewer:to_proplist(record_info(fields,loaded_mod),Info),
    Title = io_lib:format("~s",[Info#loaded_mod.mod]),
    {ok,{Title,Proplist,TW}}.

detail_pages() ->
    [{"General Information",      fun init_gen_page/2},
     {"Current Attributes",       fun init_curr_attr_page/2},
     {"Current Compilation Info", fun init_curr_comp_page/2},
     {"Old Attributes",           fun init_old_attr_page/2},
     {"Old Compilation Info",     fun init_old_comp_page/2}].

init_gen_page(Parent, Info) ->
    Fields = info_fields(),
    cdv_info_wx:start_link(Parent,{Fields,Info,[]}).

init_curr_attr_page(Parent, Info) ->
    init_info_page(Parent, proplists:get_value(current_attrib,Info)).

init_curr_comp_page(Parent, Info) ->
    init_info_page(Parent, proplists:get_value(current_comp_info,Info)).

init_old_attr_page(Parent, Info) ->
    init_info_page(Parent, proplists:get_value(old_attrib,Info)).

init_old_comp_page(Parent, Info) ->
    init_info_page(Parent, proplists:get_value(old_comp_info,Info)).

init_info_page(Parent, undefined) ->
    init_info_page(Parent, "");
init_info_page(Parent, String) ->
    cdv_html_wx:start_link(Parent,observer_html_lib:plain_page(String)).

format({Bin,q}) when is_binary(Bin) ->
    [$'|binary_to_list(Bin)];
format({Bin,nq}) when is_binary(Bin) ->
    lists:flatten(io_lib:format("~ts",[Bin]));
format(D) ->
    D.

%%%-----------------------------------------------------------------
%%% Internal
info_fields() ->
    [{"Overview",
      [{"Name",                     mod},
       {"Current Size",             current_size},
       {"Old Size",                 old_size}]}].
