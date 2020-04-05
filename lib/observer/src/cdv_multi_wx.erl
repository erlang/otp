%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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
-module(cdv_multi_wx).

-behaviour(wx_object).

-export([start_link/2]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

%% Records
-record(state,
	{main_panel,
	 main_sizer,
	 menu,
	 menu_sizer,
	 callback,
	 pages,
	 dyn_panel,
	 dyn_sizer,
	 dyn_page
	}).

start_link(Notebook, Info) ->
    wx_object:start_link(?MODULE, [Notebook, Info], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, Callback]) when is_atom(Callback) ->
    Pages = Callback:get_info(),
    {MainPanel,State0} = init([Notebook, Pages]),
    {MainPanel,State0#state{callback=Callback}};
init([Notebook, Pages]) ->
    MainPanel = wxPanel:new(Notebook),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    LeftMenuSizer = wxStaticBoxSizer:new(?wxVERTICAL,MainPanel,
					 [{label,"Please select"}]),
    LeftMenu = wxListBox:new(MainPanel,?wxID_ANY,
			     [{style,?wxLB_SINGLE},
			      {choices,[T || {T,_,_} <- Pages]}]),
    wxListBox:setSelection(LeftMenu,0),
    wxListBox:connect(LeftMenu, command_listbox_selected),
    wxSizer:add(LeftMenuSizer,LeftMenu,[{flag,?wxEXPAND},{proportion,2}]),

    DynPanel  = wxScrolledWindow:new(MainPanel),
    wxScrolledWindow:enableScrolling(DynPanel,true,true),
    wxScrolledWindow:setScrollbars(DynPanel,1,1,0,0),

    BorderFlags = ?wxLEFT bor ?wxRIGHT,
    wxSizer:add(Sizer, LeftMenuSizer,
		[{flag, ?wxEXPAND bor BorderFlags bor ?wxTOP},
		 {proportion, 0}, {border, 5}]),
    wxSizer:add(Sizer, DynPanel, [{flag, ?wxEXPAND bor BorderFlags bor ?wxTOP},
				  {proportion, 1}, {border, 5}]),
    wxPanel:setSizer(MainPanel, Sizer),

    State = load_dyn_page(#state{main_panel=MainPanel,
				 main_sizer=Sizer,
				 menu=LeftMenu,
				 menu_sizer=LeftMenuSizer,
				 pages=Pages,
				 dyn_panel=DynPanel
				}),
    {MainPanel, State}.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(active, State) ->
    NewState =
	wx:batch(
	  fun() ->
		  update_dyn_page(State)
	  end),
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("~p:~p: Unhandled info: ~tp~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

handle_call(new_dump, _From, State) ->
    NewState =
	wx:batch(
	  fun() ->
		  update_left_menu(State)
	  end),
    {reply, ok, NewState};

handle_call(Msg, _From, State) ->
    io:format("~p:~p: Unhandled Call ~tp~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p:~p: Unhandled cast ~tp~n",[?MODULE, ?LINE, Msg]),
    {noreply, State}.

handle_event(#wx{event=#wxCommand{type=command_listbox_selected,
				  cmdString=[]}},
	     State) ->
    %% For some reason, the listbox sometimes gets an "unselect"
    %% command like this during termination. Ignore!
    {noreply, State};

handle_event(#wx{event=#wxCommand{type=command_listbox_selected,
				  cmdString=_DynName}},
	     State) ->
    NewState =
	wx:batch(fun() ->
			 update_dyn_page(State)
		 end),
    {noreply,NewState};

handle_event(Event, State) ->
    io:format("~p:~p: Unhandled event ~tp\n", [?MODULE,?LINE,Event]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_left_menu(#state{main_panel=Panel,
			callback=Callback,
			menu=OldMenu,
			menu_sizer=MenuSizer} = State) ->
    Pages = Callback:get_info(),
    wxListBox:disconnect(OldMenu),
    wxWindow:destroy(OldMenu),
    NewMenu = wxListBox:new(Panel,?wxID_ANY,
			    [{style,?wxLB_SINGLE},
			     {choices,[T || {T,_,_} <- Pages]}]),
    wxListBox:setSelection(NewMenu,0),
    wxListBox:connect(NewMenu, command_listbox_selected),
    wxSizer:add(MenuSizer,NewMenu,[{flag,?wxEXPAND},{proportion,2}]),
    wxSizer:layout(MenuSizer),
    State#state{pages=Pages,menu=NewMenu}.

update_dyn_page(#state{dyn_page=undefined} = State) ->
    load_dyn_page(State);
update_dyn_page(#state{dyn_page=OldDynPage,
		       dyn_sizer=OldDynSizer} = State) ->
    wxSizer:detach(OldDynSizer,OldDynPage),
    wxWindow:destroy(OldDynPage),
    load_dyn_page(State).

load_dyn_page(#state{main_sizer=MainSizer,
		     dyn_panel=DynPanel,
		     menu=Menu,
		     pages=Pages} = State) ->
    %% Freeze and thaw causes a hang (and is not needed) on 2.9 and higher
    DoFreeze = [?wxMAJOR_VERSION,?wxMINOR_VERSION] < [2,9],
    DoFreeze andalso wxWindow:freeze(DynPanel),
    Name = wxListBox:getStringSelection(Menu),
    {Page,Sizer} = load_dyn_page(DynPanel,Name,Pages),
    wxSizer:layout(MainSizer),
    DoFreeze andalso wxWindow:thaw(DynPanel),
    wx_object:get_pid(Page) ! active,
    State#state{dyn_page=Page,dyn_sizer=Sizer}.

load_dyn_page(Panel,Name,Pages) ->
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label,Name}]),

    {_,Callback,Info} = lists:keyfind(Name,1,Pages),
    DynPage = Callback:start_link(Panel,Info),

    wxSizer:add(Sizer,DynPage,[{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizerAndFit(Panel,Sizer,[{deleteOld,true}]),
    {DynPage,Sizer}.
