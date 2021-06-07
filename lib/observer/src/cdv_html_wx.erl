%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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
-module(cdv_html_wx).

-behaviour(wx_object).

-export([start_link/2]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

%% Records
-record(state,
	{parent,
         panel,
	 app,         %% which tool is the user
	 expand_table,
         expand_wins=[],
         delayed_fetch,
         trunc_warn=[]}).

start_link(ParentWin, Info) ->
    wx_object:start_link(?MODULE, [ParentWin, Info], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentWin, Callback]) when is_atom(Callback) ->
    init(ParentWin, Callback);
init([ParentWin, {App, Fun}]) when is_function(Fun) ->
    init([ParentWin, {App, Fun()}]);
init([ParentWin, {expand,HtmlText,Tab}]) ->
    init(ParentWin, HtmlText, Tab, cdv);
init([ParentWin, {App, {expand,HtmlText,Tab}}]) ->
    init(ParentWin, HtmlText, Tab, App);
init([ParentWin, {App,HtmlText}]) ->
    init(ParentWin, HtmlText, undefined, App);
init([ParentWin, HtmlText]) ->
    init(ParentWin, HtmlText, undefined, cdv).

init(ParentWin, HtmlText, Tab, App) ->
    %% If progress dialog is shown, remove it now - and sett cursor busy instead
    observer_lib:destroy_progress_dialog(),
    wx_misc:beginBusyCursor(),
    HtmlWin = observer_lib:html_window(ParentWin),
    wxHtmlWindow:setPage(HtmlWin,HtmlText),
    wx_misc:endBusyCursor(),
    {HtmlWin, #state{parent=ParentWin, panel=HtmlWin,expand_table=Tab,app=App}}.

init(ParentWin, Callback) ->
    {HtmlWin, State} = init(ParentWin, "", undefined, cdv),
    {HtmlWin, State#state{delayed_fetch=Callback}}.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(active, #state{parent=Parent, panel=HtmlWin,delayed_fetch=Callback}=State)
  when Callback=/=undefined ->
    observer_lib:display_progress_dialog(HtmlWin,
                                         "Crashdump Viewer",
                                         "Reading data"),
    {{expand,Title,Info,Tab},TW} = Callback:get_info(),
    Cs = observer_lib:colors(Parent),
    HtmlText = observer_html_lib:expandable_term(Title,Info,Tab,Cs),

    observer_lib:sync_destroy_progress_dialog(),
    wx_misc:beginBusyCursor(),
    wxHtmlWindow:setPage(HtmlWin,HtmlText),
    cdv_wx_set_status(State, TW),
    wx_misc:endBusyCursor(),
    {noreply, State#state{expand_table=Tab,
                          delayed_fetch=undefined,
                          trunc_warn=TW}};

handle_info(active, State) ->
    cdv_wx_set_status(State, State#state.trunc_warn),
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

handle_cast({detail_win_closed, Id},#state{expand_wins=Opened0}=State) ->
    Opened = lists:keydelete(Id, 1, Opened0),
    {noreply, State#state{expand_wins=Opened}};

handle_cast(Msg, State) ->
    io:format("~p~p: Unhandled cast ~tp~n",[?MODULE, ?LINE, Msg]),
    {noreply, State}.

handle_event(#wx{event=#wxHtmlLink{type=command_html_link_clicked,
				   linkInfo=#wxHtmlLinkInfo{href=Target}}},
	     #state{expand_table=Tab, app=App}=State) ->
    NewState=
	case Target of
	    "#Binary?" ++ BinSpec ->
		[{"offset",Off},{"size",Size},{"pos",Pos}] =
		    uri_string:dissect_query(BinSpec),
		Id = {cdv, {list_to_integer(Off),
			    list_to_integer(Size),
			    list_to_integer(Pos)}},
		expand(Id,cdv_bin_cb,State);
	    "#OBSBinary?" ++ BinSpec ->
		[{"key1",Preview},{"key2",Size},{"key3",Hash}] =
		    uri_string:dissect_query(BinSpec),
		Id = {obs, {Tab, {list_to_integer(Preview),
				  list_to_integer(Size),
				  list_to_integer(Hash)}}},
		expand(Id,cdv_bin_cb,State);
	    "#Term?" ++ TermKeys ->
		[{"key1",Key1},{"key2",Key2},{"key3",Key3}] =
		    uri_string:dissect_query(TermKeys),
		Id = {cdv, {Tab,{list_to_integer(Key1),
				 list_to_integer(Key2),
				 list_to_integer(Key3)}}},
		expand(Id,cdv_term_cb,State);
	    _ when App =:= obs ->
		observer ! {open_link, Target},
                State;
	    _ ->
		cdv_virtual_list_wx:start_detail_win(Target),
		State
	end,
    {noreply, NewState};

handle_event(Event, State) ->
    io:format("~p:~p: Unhandled event ~tp\n", [?MODULE,?LINE,Event]),
    {noreply, State}.

%%%-----------------------------------------------------------------
%%% Internal
expand(Id,Callback,#state{expand_wins=Opened0, app=App}=State) ->
    Opened =
	case lists:keyfind(Id,1,Opened0) of
	    false ->
		EW = cdv_detail_wx:start_link(Id,[],State#state.panel,
                                              Callback,App),
		wx_object:get_pid(EW) ! active,
		[{Id,EW}|Opened0];
	    {_,EW} ->
		wxFrame:raise(EW),
		Opened0
	end,
    State#state{expand_wins=Opened}.

cdv_wx_set_status(#state{app = cdv}, Status) ->
    %% this module is used by the observer when cdw_wx isn't started
    %% only try to set status when used by cdv
    cdv_wx:set_status(Status);
cdv_wx_set_status(_, _) ->
    ok.
