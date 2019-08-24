%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(ex_popupMenu).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config,
	  menu
	}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Root = proplists:get_value(parent, Config),
    Parent = wxPanel:new(Root,[]),    
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Box = wxStaticBox:new(Parent, ?wxID_ANY, "Popup Menu"),
    Sz = wxStaticBoxSizer:new(Box, ?wxVERTICAL),
    Text = wxStaticText:new(Parent, ?wxID_ANY, "Right click to open popup menu", []),
    Panel = wxPanel:new(Parent),
    wxPanel:connect(Panel, right_up),
    Sizer = wxBoxSizer:new(?wxVERTICAL),    
    wxSizer:add(Sizer, Text, [{border, 20}, {flag, ?wxALL}]),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:add(Sz, Panel, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxSizer:layout(Sz),
    PopupMenu = create_menu(),
    wxSizer:add(MainSizer, Sz, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Parent, MainSizer),
    {Parent, #state{parent=Parent, config=Config, menu=PopupMenu}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{obj = Panel,
		 event = #wxMouse{type = right_up}},
	     State = #state{menu = Menu}) ->
    %% Open the popup menu
    wxWindow:popupMenu(Panel, Menu),
    {noreply, State};
handle_event(#wx{obj = Menu, id = Id,
		 event = #wxCommand{type = command_menu_selected}},
	     State = #state{}) ->
    %% Get the selected item label
    Label = wxMenu:getLabel(Menu, Id),
    demo:format(State#state.config, "wxMenu clicked ~p\n", [Label]),
    {noreply, State};
handle_event(Ev, State) ->
    demo:format(State#state.config, "Unexpected Event ~p\n", [Ev]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.


code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, #state{menu=Popup}) ->
    wxMenu:destroy(Popup),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menu() ->
    Menu = wxMenu:new([]),
    SubMenu  = wxMenu:new([]),
    SubMenu2 = wxMenu:new([]),

    wxMenu:append(Menu, ?wxID_UNDO, "Undo", []),
    wxMenu:append(Menu, ?wxID_REDO, "Redo", []),
    wxMenu:append(Menu, ?wxID_HELP, "Help", []),
    wxMenu:appendSeparator(Menu),
    wxMenu:appendCheckItem(Menu, ?wxID_ANY, "Check item", []),
    wxMenu:appendSeparator(Menu),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 1", []),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 2", []),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 3", []),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 4", []),

    wxMenu:appendSeparator(Menu),
    wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Disabled", []), [{enable,false}]),
    wxMenu:appendSeparator(Menu),

    wxMenu:append(SubMenu, ?wxID_ABOUT, "About", []),
    wxMenu:append(SubMenu, ?wxID_ANY, "Sub Item2", []),
    wxMenu:append(SubMenu, ?wxID_SAVE, "Save", []),
    wxMenu:break(SubMenu),
    wxMenu:append(SubMenu, ?wxID_EXIT, "Exit", []),
    wxMenu:append(SubMenu, ?wxID_OPEN, "Open", []),
    wxMenu:append(SubMenu, ?wxID_NEW, "New", []),
    wxMenu:append(Menu, ?wxID_ANY, "Sub menu", SubMenu, []),

    wxMenu:appendCheckItem(SubMenu2, ?wxID_ANY, "Check Item", []),
    wxMenu:appendSeparator(SubMenu2),
    wxMenu:append(SubMenu2, ?wxID_CLEAR, "Clear", []),
    wxMenu:append(SubMenu2, ?wxID_ANY, "Sub Item", []),

    Bitmap = wxArtProvider:getBitmap("wxART_NEW"),
    AnotherSubMenu = wxMenuItem:new([{parentMenu, Menu},
				     {id, ?wxID_ANY},
				     {text, "Another sub menu"},
				     {subMenu, SubMenu2},
				     {kind, ?wxITEM_NORMAL}]),
    wxMenuItem:setBitmap(AnotherSubMenu, Bitmap),
    wxMenu:append(Menu, AnotherSubMenu),

    wxMenu:connect(Menu, command_menu_selected),
    wxMenu:connect(SubMenu, command_menu_selected),
    wxMenu:connect(SubMenu2, command_menu_selected),
    Menu.
