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

-module(ex_treeCtrl).

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
	  config
	}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "wxTreeCtrl"}]),

    %% Setup treeCtrl
    TreeCtrl = wxTreeCtrl:new(Panel, []),
    RootId = wxTreeCtrl:addRoot(TreeCtrl, "Root"),
    %% Name the first items
    Items = ["item "++integer_to_list(Int)||
		Int <- lists:seq(1,10)],
    %% Create the first items in the treeCtrl
    SubItems = [{wxTreeCtrl:appendItem(TreeCtrl, RootId, Item), Item}||
		   Item <- Items],
    %% Create sub items
    [wxTreeCtrl:appendItem(TreeCtrl, ItemId, Item++" sub item "++integer_to_list(Int))||
	 {ItemId, Item} <- SubItems, Int <- lists:seq(1,10)],
    wxTreeCtrl:expand(TreeCtrl, RootId),

    %% Add to sizers
    Options = [{flag, ?wxEXPAND}, {proportion, 1}],
    wxSizer:add(Sizer, TreeCtrl, Options),
    wxSizer:add(MainSizer, Sizer, Options),

    wxTreeCtrl:connect(TreeCtrl, command_tree_item_collapsed),
    wxTreeCtrl:connect(TreeCtrl, command_tree_item_expanded),
    wxTreeCtrl:connect(TreeCtrl, command_tree_sel_changed),
    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxTree{type = command_tree_item_collapsed,
				 item = Item},
		 obj = TreeCtrl},
	     State = #state{}) ->
    ItemText = wxTreeCtrl:getItemText(TreeCtrl, Item),
    demo:format(State#state.config, "You have collapsed ~p.\n", [ItemText]),
    {noreply, State};
handle_event(#wx{event = #wxTree{type = command_tree_item_expanded,
				 item = Item},
		 obj = TreeCtrl},
	     State = #state{}) ->
    ItemText = wxTreeCtrl:getItemText(TreeCtrl, Item),
    demo:format(State#state.config, "You have expanded ~p.\n", [ItemText]),
    {noreply, State};
handle_event(#wx{event = #wxTree{type = command_tree_sel_changed,
				 item = Item},
		 obj = TreeCtrl},
	     State = #state{}) ->
    ItemText = wxTreeCtrl:getItemText(TreeCtrl, Item),
    demo:format(State#state.config, "You have selected ~p.\n", [ItemText]),
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

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

