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
-module(cdv_info_wx).

-behaviour(wx_object).

-export([start_link/2]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

%% Records
-record(state,
	{panel,
	 sizer,
	 fpanel,
	 callback,
	 trunc_warn=[]
	}).

start_link(ParentWin, Info) ->
    wx_object:start_link(?MODULE, [ParentWin, Info], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentWin, Callback]) when is_atom(Callback) ->
    {InfoFields,Info,TW} = Callback:get_info(),
    {Panel,Sizer,FPanel} = create_box(ParentWin,InfoFields,Info),
    {Panel,#state{panel=Panel,
		  sizer=Sizer,
		  fpanel=FPanel,
		  callback=Callback,
		  trunc_warn=TW}};

init([ParentWin, {InfoFields,Info,TW}]) ->
    {Panel,Sizer,FPanel} = create_box(ParentWin,InfoFields,Info),
    {Panel, #state{panel=Panel,
		   sizer=Sizer,
		   fpanel=FPanel,
		   trunc_warn=TW}}.

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

handle_call(new_dump, _From, #state{callback=Callback,panel=Panel,
				    sizer=Sizer,fpanel=FPanel} = State) ->
    {InfoFields,Info,TW} = Callback:get_info(),
    NewFPanel =
	wx:batch(
	  fun() ->
		  wxWindow:destroy(FPanel),
		  FP = create_field_panel(Panel,Sizer,InfoFields,Info),
		  wxSizer:layout(Sizer),
		  FP
	  end),
    {reply, ok, State#state{fpanel=NewFPanel,trunc_warn=TW}};

handle_call(Msg, _From, State) ->
    io:format("~p~p: Unhandled Call ~tp~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p~p: Unhandled cast ~tp~n",[?MODULE, ?LINE, Msg]),
    {noreply, State}.

handle_event(#wx{obj=MoreEntry,event=#wxMouse{type=left_down},userData={more,More}}, State) ->
    observer_lib:add_scroll_entries(MoreEntry,More),
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=left_down},userData=Target}, State) ->
    cdv_virtual_list_wx:start_detail_win(Target),
    {noreply, State};

handle_event(#wx{obj=Obj,event=#wxMouse{type=enter_window}},State) ->
    wxTextCtrl:setForegroundColour(Obj,{0,0,100,255}),
    {noreply, State};

handle_event(#wx{obj=Obj,event=#wxMouse{type=leave_window}},State) ->
    wxTextCtrl:setForegroundColour(Obj,?wxBLUE),
    {noreply, State};

handle_event(Event, State) ->
    io:format("~p:~p: Unhandled event ~tp\n", [?MODULE,?LINE,Event]),
    {noreply, State}.

%%%-----------------------------------------------------------------
%%% Internal
create_box(ParentWin,InfoFields,Info) ->
    Panel = wxPanel:new(ParentWin),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    FPanel = create_field_panel(Panel,Sizer,InfoFields,Info),
    wxPanel:setSizer(Panel, Sizer),
    {Panel,Sizer,FPanel}.

create_field_panel(Panel,Sizer,InfoFields,Info0) ->
    Info = observer_lib:fill_info(InfoFields, Info0),
    {FPanel, _FSizer, _Fields} = observer_lib:display_info(Panel,Info),
    BorderFlags = ?wxLEFT bor ?wxRIGHT,
    wxSizer:add(Sizer, FPanel, [{flag, ?wxEXPAND bor BorderFlags bor ?wxTOP},
				{proportion, 0}, {border, 5}]),
    FPanel.
