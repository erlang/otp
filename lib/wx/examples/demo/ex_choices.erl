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

-module(ex_choices).

-behaviour(wx_object).

-export([start/1, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config,
	  list_box
	 }).


start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
        wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxScrolledWindow:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    ListBoxSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "wxListBox"}]),

    ChoiceSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "wxChoice"}]),
    SpinSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				     [{label, "wxSpinCtrl"}]),
    ComboSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				     [{label, "wxComboBox"}]),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    Sizer3  = wxBoxSizer:new(?wxHORIZONTAL),

    Choices = ["one","two","three",
	       "four","five","six",
	       "seven","eight","nine",
	       "ten", "eleven", "twelve"],

    %% Create a wxListBox that uses multiple selection
    ListBox = wxListBox:new(Panel, 1, [{size, {-1,100}},
				       {choices, ["Multiple selection"|Choices]},
				       {style, ?wxLB_MULTIPLE}]),
    wxListBox:setToolTip(ListBox, "A wxListBox with multiple selection"),

    %% Create a wxListBox that uses single selection
    ListBox2 = wxListBox:new(Panel, 2, [{size, {-1,100}},
					{choices, ["Single selection"|Choices]},
					{style, ?wxLB_SINGLE}]),
    wxListBox:setToolTip(ListBox2, "A wxListBox with single selection"),

    %% Create a wxChoice
    Choice = wxChoice:new(Panel, 4, [{choices, Choices}]),
    wxChoice:setToolTip(Choice, "A wxChoice"),

    %% Create a wxSpinCtrl with range between 0 and 100
    SpinCtrl = wxSpinCtrl:new(Panel, []),
    wxSpinCtrl:setRange(SpinCtrl, 0, 100),
    wxSpinCtrl:setToolTip(SpinCtrl, "A wxSpinCtrl with range from 0 to 100"),

    %% Create a wxComboBox and set the value to "Default value"
    ComboBox = wxComboBox:new(Panel, 5, [{choices, Choices}]),
    wxComboBox:setToolTip(ComboBox, "A wxComboBox"),


    wxChoice:connect(Choice,command_choice_selected),
    wxSpinCtrl:connect(SpinCtrl,command_spinctrl_updated),
    wxComboBox:connect(ComboBox, command_combobox_selected),


    %% Add to sizers
    Options = [{border,4}, {flag, ?wxALL}],
    wxSizer:add(Sizer, ListBox, Options),
    wxSizer:add(Sizer, ListBox2, Options),

    wxSizer:add(ChoiceSizer, Choice, Options),
    wxSizer:add(SpinSizer, SpinCtrl, Options),
    wxSizer:add(Sizer3, ChoiceSizer, []),
    wxSizer:add(Sizer3, SpinSizer, [{border, 4}, {flag, ?wxLEFT}]),

    wxSizer:add(ComboSizer, ComboBox, Options),

    wxSizer:add(ListBoxSizer, Sizer, Options),
    wxSizer:add(MainSizer, ListBoxSizer, Options),
    wxSizer:add(MainSizer, Sizer3, Options),
    wxSizer:add(MainSizer, ComboSizer, Options),

    wxScrolledWindow:setScrollRate(Panel, 5, 5),
    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		   list_box = ListBox}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{obj = ComboBox,
		 event = #wxCommand{type = command_combobox_selected}},
	     State = #state{}) ->
    Value = wxComboBox:getValue(ComboBox),
    demo:format(State#state.config,"Selected wxComboBox ~p\n",[Value]),
    {noreply, State};
handle_event(#wx{event = #wxCommand{type = command_choice_selected,
					cmdString = Value}},
	     State = #state{}) ->
    demo:format(State#state.config,"Selected wxChoice ~p\n",[Value]),
    {noreply, State};
handle_event(#wx{event = #wxSpin{type = command_spinctrl_updated,
				 commandInt = Int}},
	     State = #state{}) ->
    demo:format(State#state.config,"wxSpinCtrl changed to ~p\n",[Int]),
    {noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config,"Got Event ~p\n",[Ev]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n",[Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config,"Got Call ~p\n",[Msg]),
    {reply, {error,nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

