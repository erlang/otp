%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-module(ex_aui).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_event/2]).

-include("../../include/wx.hrl").

-record(state, 
	{
	  parent,
	  config
	}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

-define(pi, wxAuiPaneInfo).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    Manager = wxAuiManager:new([{managed_wnd, Panel}
				]),

    Pane = ?pi:new(),
    ?pi:closeButton(Pane),
    ?pi:right(Pane),
    ?pi:dockable(Pane, [{b, true}]),
    ?pi:floatingSize(Pane, 300,200),
    ?pi:minSize(Pane, {50,50}),
    ?pi:paneBorder(Pane),
    ?pi:floatable(Pane, [{b, true}]),

    create_pane(Panel, Manager, Pane),
    create_pane(Panel, Manager,
		?pi:caption(?pi:top(?pi:new(Pane)), "One")),
    create_pane(Panel, Manager,
		?pi:caption(?pi:left(?pi:new(Pane)), "two")),
    create_pane(Panel, Manager,
		?pi:caption(?pi:bottom(?pi:new(Pane)), "Three")),
    Pane2 = wxAuiPaneInfo:new(Pane),
    ?pi:centrePane(Pane2),
    create_notebook(Panel, Manager, ?pi:new(Pane2)),

    wxPanel:setSizer(Panel, MainSizer),

    wxAuiManager:connect(Manager, aui_pane_button, [{skip,true}]),
    wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
    wxAuiManager:update(Manager),

    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{obj = Notebook,
		 event = #wxCommand{type = command_button_clicked}},
	     State) ->
    Tab = wxPanel:new(Notebook, []),
    wxButton:new(Tab, ?wxID_ANY, [{label,"New tab"}]),
    wxAuiNotebook:insertPage(Notebook, 1, Tab, "OMG TAB!! ", [{select, false}]),
    {noreply, State};
handle_event(#wx{obj = Notebook,
		 event = #wxAuiNotebook{type = command_auinotebook_page_changed,
					selection = Sel}}, State) ->
    demo:format(State#state.config, "You have changed page to ~p.\n",
		[wxAuiNotebook:getPageText(Notebook, Sel)]),
    {noreply, State};
handle_event(#wx{event = #wxAuiNotebook{type = command_auinotebook_page_close}}, State) ->
    demo:format(State#state.config, "You have closed a page.\n",[]),
    {noreply, State};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_button,
				       button = Button}}, State) ->
    case Button of
	?wxAUI_BUTTON_CLOSE ->
	    demo:format(State#state.config, "You have closed a pane.\n",[]);
	?wxAUI_BUTTON_MAXIMIZE_RESTORE ->
	    ok;
	?wxAUI_BUTTON_PIN ->
	    demo:format(State#state.config, "You have pinned a pane.\n",[])
    end,
    {noreply, State};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_maximize}}, State) ->
    demo:format(State#state.config, "You have maximized a pane.\n",[]),
    {noreply, State};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_restore}}, State) ->
    demo:format(State#state.config, "You have restored a pane.\n",[]),
    {noreply, State};
handle_event(Ev = #wx{}, State) ->
    io:format("~p\n", [Ev]),
    {noreply, State}.


code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


create_notebook(Parent, Manager, Pane) ->
    Style = (0
	     bor ?wxAUI_NB_DEFAULT_STYLE
	     bor ?wxAUI_NB_TOP
	     bor ?wxAUI_NB_WINDOWLIST_BUTTON
	     bor ?wxAUI_NB_CLOSE_ON_ACTIVE_TAB
	     bor ?wxAUI_NB_TAB_MOVE
	     bor ?wxAUI_NB_SCROLL_BUTTONS
	    ),
    
    Notebook = wxAuiNotebook:new(Parent, [{style, Style}]),

    Tab1 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Tab1, ?wxBLACK),
    wxButton:new(Tab1, ?wxID_ANY, [{label,"New tab"}]),
    wxAuiNotebook:addPage(Notebook, Tab1, "You can", []),

    Tab2 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Tab2, ?wxRED),
    wxButton:new(Tab2, ?wxID_ANY, [{label,"New tab"}]),
    wxAuiNotebook:addPage(Notebook, Tab2, "rearrange", []),

    Tab3 = wxPanel:new(Notebook, []),
    wxPanel:setBackgroundColour(Tab3, ?wxGREEN),
    wxButton:new(Tab3, ?wxID_ANY, [{label,"New tab"}]),
    wxAuiNotebook:addPage(Notebook, Tab3, "these tabs", []),

    wxAuiManager:addPane(Manager, Notebook, Pane),

    wxAuiNotebook:connect(Notebook, command_button_clicked),
    wxAuiNotebook:connect(Notebook, command_auinotebook_page_close, [{skip, false}]),
    wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed),
    Notebook.


create_pane(Parent, Manager, Pane) ->
    TextCtrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{size, {300,200}},
						  {value, "An empty pane"},
						  {style, 0
						   bor ?wxDEFAULT
						   bor ?wxTE_MULTILINE}]),
    wxAuiManager:addPane(Manager, TextCtrl, Pane),
    TextCtrl.
    
    
