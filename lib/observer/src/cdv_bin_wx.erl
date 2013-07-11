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
-module(cdv_bin_wx).

-export([get_details/1,
	 detail_pages/0]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").

%% Callbacks for cdv_detail_win
get_details(Id) ->
    {ok,Bin} = crashdump_viewer:expand_binary(Id),
    {ok,{"Expanded Binary", io_lib:format("~tp",[Bin]), []}}.

detail_pages() ->
    [{simple, "Binary", fun init_bin_page/3}].

init_bin_page(Parent, _, Bin) ->
    Html = crashdump_viewer_html:plain_page(Bin),
    observer_lib:html_window(Parent,Html).
