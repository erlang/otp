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
-module(cdv_table_wx).

-behaviour(wx_object).

-export([start_link/2]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

%% Records
-record(state,
	{trunc_warn=[]}).

start_link(ParentWin, Info) ->
    wx_object:start_link(?MODULE, [ParentWin, Info], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentWin, Callback]) when is_atom(Callback) ->
    {ok,TableInfo} = Callback:get_info(),
    init([ParentWin, TableInfo]);

init([ParentWin, {ColumnSpec,Info,TW}]) ->
    Style0 = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES bor ?wxLC_VRULES,
    Style =
	case lists:all(fun({"",_,_}) -> true; (_) -> false end, ColumnSpec) of
	    true -> Style0 bor ?wxLC_NO_HEADER;
	    false -> Style0
	end,
    Grid = wxListCtrl:new(ParentWin, [{style, Style}]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, DefSize),
			   Col + 1
		   end,
    lists:foldl(AddListEntry, 0, ColumnSpec),
    wxListItem:destroy(Li),
    Insert = fun(RowData, Row) ->
		     wxListCtrl:insertItem(Grid, Row, ""),
		     set_items(Grid,Row,RowData,0),
		     Row + 1
	     end,
    lists:foldl(Insert, 0, Info),
    {Grid, #state{trunc_warn=TW}}.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(active, State) ->
    cdv_wx:set_status(State#state.trunc_warn),
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p:~p: Unhandled info: ~tp~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

handle_call(Msg, _From, State) ->
    io:format("~p~p: Unhandled Call ~tp~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p~p: Unhandled cast ~tp~n",[?MODULE, ?LINE, Msg]),
    {noreply, State}.

handle_event(Event, State) ->
    io:format("~p:~p: Unhandled event ~tp\n", [?MODULE,?LINE,Event]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_items(Grid,Row,[Col|Cols],ColN) ->
    Str = case Col of
	      undefined -> "";
	      _ -> observer_lib:to_str(Col)
	  end,
    wxListCtrl:setItem(Grid, Row, ColN, Str),
    set_items(Grid,Row,Cols,ColN+1);
set_items(_,_,[],_) ->
    ok.
