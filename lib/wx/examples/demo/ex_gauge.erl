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

-module(ex_gauge).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

-export([start/1, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(gauge, {obj,
		value,
		timer,
		range
	       }).

-record(state, 
	{
	  parent,
	  config,
	  normal_gauge,
	  undeterminate_gauge,
	  simulate_button,
	  simulate_undet_button
	 }).

-define(ID_START_STOP, 1).
-define(ID_START_STOP_UNDET, 2).
-define(SET_VALUE, 3).
-define(VERTICAL, 4).

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
    AlignSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				      [{label, "wxGauge"}]),

    %% Create two horizontal gauges with range 0-100
    Range = 100,
    NormalGauge = wxGauge:new(Panel, 1, Range, [{size, {200, -1}},
						{style, ?wxGA_HORIZONTAL}]),
    UndetGauge = wxGauge:new(Panel, 2, Range, [{size, {200, -1}},
					       {style, ?wxGA_HORIZONTAL}]),
    
    %% Add to sizers
    wxSizer:add(AlignSizer, NormalGauge, [{flag, ?wxEXPAND}]),
    wxSizer:add(AlignSizer, UndetGauge, [{flag, ?wxEXPAND}]),
    {OptSizer, SimulateButton, SimulateUndetermButton}
	= create_option_sizer(Panel),

    wxSizer:add(MainSizer, AlignSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, OptSizer, []),

    wxWindow:connect(Panel, command_togglebutton_clicked),
    wxWindow:connect(Panel, command_button_clicked),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		   normal_gauge = #gauge{obj=NormalGauge, value = 0,
					 range = Range},
		   undeterminate_gauge = #gauge{obj=UndetGauge, value = 0,
					range = Range},
		   simulate_button = SimulateButton,
		   simulate_undet_button = SimulateUndetermButton}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks handled as normal gen_server callbacks
handle_info(tick, State=#state{normal_gauge = Gauge=#gauge{obj = Obj,
							   value = Value,
							   timer = Timer,
							   range = Range},
			       simulate_button = Button}) ->
    Timer2 =
	if Value >= Range ->
		wxToggleButton:setValue(Button, false),
		wxToggleButton:setLabel(Button, "Simulate load"),
		wxGauge:setValue(Obj, Range),
		demo:format(State#state.config, "Simulation finished.\n", []),
		Timer;
	   true ->
		simulate_load(Obj, Value)

	end,
    {noreply, State#state{normal_gauge = Gauge#gauge{value = Value+5,
						     timer = Timer2}}};
handle_info(pulse, State=#state{undeterminate_gauge = Gauge=#gauge{obj = Obj}}) ->
    wxGauge:pulse(Obj),
    Timer = erlang:send_after(300, self(), pulse),
    {noreply, State#state{undeterminate_gauge = Gauge#gauge{timer = Timer}}}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config,"Got Call ~p\n",[Msg]),
    {reply,ok, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{id = ?ID_START_STOP,
		 event = #wxCommand{type = command_togglebutton_clicked,
				    commandInt = Int}},
	     State = #state{normal_gauge = #gauge{obj=Gauge,
						  value = Value,
						  timer = Timer},
			    simulate_button = Button}) ->
    case Int of
	1 ->
	    wxToggleButton:setLabel(Button, "Stop simulation"),
	    simulate_load(Gauge, Value);
	0 ->
	    erlang:cancel_timer(Timer),
	    wxToggleButton:setLabel(Button, "Simulate load"),
	    ok
    end,
    {noreply, State};
handle_event(#wx{id = ?ID_START_STOP_UNDET,
		 event = #wxCommand{type = command_togglebutton_clicked,
				    commandInt = Int}},
	     State = #state{undeterminate_gauge = G = #gauge{obj=Gauge,
						    timer = Timer},
			    simulate_undet_button = Button}) ->
    Timer2 =
	case Int of
	    1 ->
		wxToggleButton:setLabel(Button, "Stop simulation"),
		wxGauge:pulse(Gauge),
		erlang:send_after(300, self(), pulse);
	    0 ->
		wxToggleButton:setLabel(Button, "Simulate undeterminate load"),
		reset(G),
		undefined
    end,
    case Timer of
	undefined -> ok;
	_         -> erlang:cancel_timer(Timer)
    end,
    {noreply, State#state{undeterminate_gauge = G#gauge{timer = Timer2}}};
handle_event(#wx{id = ?wxID_CLEAR,
		 event = #wxCommand{type = command_button_clicked}},
	    State = #state{normal_gauge = Gauge,
			   undeterminate_gauge = Gauge2}) ->
    reset(Gauge),
    reset(Gauge2),
    {noreply, State#state{normal_gauge = Gauge#gauge{value = 0}}};
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config,"Got Event ~p\n",[Ev]),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simulate_load(Gauge, Value) ->
    wxGauge:setValue(Gauge, Value),
    erlang:send_after(300, self(), tick).
    
reset(#gauge{obj = Gauge}) ->    
    wxGauge:setValue(Gauge, 0).


create_option_sizer(Panel) ->
    OptSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				    [{label, "Options"}]),

    SimulateButton = wxToggleButton:new(Panel, ?ID_START_STOP, "Simulate load",
					[{size, {200, 35}}]),
    SimulateUndetermButton = wxToggleButton:new(Panel, ?ID_START_STOP_UNDET,
						"Simulate Undeterminate load",
					[{size, {200, 35}}]),
    ClearButton = wxButton:new(Panel, ?wxID_CLEAR,
			       [{size, {200, 35}}]),

    wxSizer:add(OptSizer, SimulateButton),
    wxSizer:add(OptSizer, SimulateUndetermButton),
    wxSizer:add(OptSizer, ClearButton),

    wxWindow:connect(Panel, command_checkbox_clicked),

    {OptSizer, SimulateButton, SimulateUndetermButton}.
