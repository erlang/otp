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

-module(ex_radioBox).

-behaviour(wx_object).

-export([start/1, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config,
	  radio_box
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
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),

    Choices = ["Item " ++ integer_to_list(Int) || Int <- lists:seq(1,12)],
    RadioBox = wxRadioBox:new(Panel, 1, "wxRadioBox Horizontal",
			      ?wxDefaultPosition,
			      ?wxDefaultSize,
			      Choices,
			      [{majorDim, 3},
			       {style, ?wxHORIZONTAL}]),
    wxRadioBox:connect(RadioBox, command_radiobox_selected),

    RadioButtonSizer = create_radio_buttons(Panel),

    CheckSizer = create_checkboxes(Panel),

    %% Add to sizers
    wxSizer:add(Sizer, RadioButtonSizer),
    wxSizer:addSpacer(Sizer, 20),
    wxSizer:add(Sizer, CheckSizer),

    wxSizer:add(MainSizer, RadioBox),
    wxSizer:addSpacer(MainSizer, 20),
    wxSizer:add(MainSizer, Sizer),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		   radio_box = RadioBox}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxCommand{type = command_radiobox_selected,
				    cmdString = Item}},
	     State = #state{}) ->
    demo:format(State#state.config,"wxRadioBox selected ~p\n",[Item]),
    {noreply, State};
handle_event(#wx{obj  = Checkbox,
		 event = #wxCommand{type = command_checkbox_clicked,
				    commandInt = Int}},
	     State = #state{config = Config}) ->
    Label = wxCheckBox:getLabel(Checkbox),
    case Int of
	0 -> demo:format(Config,"wxCheckBox deselected ~p\n",[Label]);
	1 -> demo:format(Config,"wxCheckBox selected ~p \n",[Label]);
	2 -> demo:format(Config,"wxCheckBox middle-state ~p\n",[Label])
    end,
    {noreply, State};
handle_event(#wx{obj  = RadioButton,
		 event = #wxCommand{type = command_radiobutton_selected}},
	     State = #state{}) ->
    Label = wxRadioButton:getLabel(RadioButton),
    demo:format(State#state.config,"wxRadioButton selected ~p\n",[Label]),
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
    {reply, {error, nyi}, State}.

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

create_checkboxes(Panel) ->
    CheckSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				      [{label, "wxCheckBox"}]),
    
    CheckBoxes =
	[wxCheckBox:new(Panel, ?wxID_ANY, "Label1", []),
	 wxCheckBox:new(Panel, ?wxID_ANY, "Label2", []),
	 wxCheckBox:new(Panel, ?wxID_ANY, "Label3", []),
	 wxCheckBox:new(Panel, ?wxID_ANY, "3-state checkbox",
			[{style, ?wxCHK_3STATE bor
			  ?wxCHK_ALLOW_3RD_STATE_FOR_USER}]),
	 wxCheckBox:new(Panel, ?wxID_ANY, "Right aligned",
			[{style, ?wxALIGN_RIGHT}])],
    Fun =
	fun(Item) ->
		wxCheckBox:connect(Item, command_checkbox_clicked),
		wxSizer:add(CheckSizer, Item)
	end,
    wx:foreach(Fun, CheckBoxes),
    CheckSizer.

    
create_radio_buttons(Panel) ->
    RadioButtonSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
					    [{label, "wxRadioButton"}]),

    Buttons =
	[wxRadioButton:new(Panel, ?wxID_ANY, "Group1 Radio1",
			   [{style, ?wxRB_GROUP}]),
	 wxRadioButton:new(Panel, ?wxID_ANY, "Group1 Radio2", []),
	 wxRadioButton:new(Panel, ?wxID_ANY, "Group1 Radio3", []),
	 wxRadioButton:new(Panel, ?wxID_ANY, "Group2 Radio1",
			   [{style, ?wxRB_GROUP}]),
	 wxRadioButton:new(Panel, ?wxID_ANY, "Group2 Radio2", []),
	 wxRadioButton:new(Panel, ?wxID_ANY, "Group2 Radio3", [])],
    Fun =
	fun(Item) ->
		wxRadioButton:connect(Item, command_radiobutton_selected),
		wxSizer:add(RadioButtonSizer, Item)
	end,

    wx:foreach(Fun, Buttons),
    RadioButtonSizer.
