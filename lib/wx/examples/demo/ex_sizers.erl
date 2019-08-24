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

-module(ex_sizers).

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
				 [{label, "wxSizer"}]),

    Choices = ["Vertical Example",
	       "Horizontal Example",
	       "Add A Strechable",
	       "More Than One Strechable",
	       "Weighting Factor",
	       "Edge Affinity",
	       "Spacer",
	       "Centering In Avalible Space",
	       "Simple Border",
	       "East And West Border",
	       "North And South Border",
	       "Box In Box",
	       "Boxes Inside A Border",
	       "Border In A Box",
	       "Simple Grid",
	       "More Grid Features",
	       "Flexible Grid",
	       "Grid With Alignment",
	       "Proportional Resize With Alignments"],

    ListBox = wxListBox:new(Panel, ?wxID_ANY, [{choices, Choices}]),
    wxListBox:connect(ListBox, command_listbox_doubleclicked),

    %% Add to sizers
    wxSizer:add(Sizer, ListBox, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxCommand{type = command_listbox_doubleclicked,
				    cmdString = Choice}},
	     State = #state{}) ->
    create_example(State#state.parent, Choice),
    {noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config, "Got Event ~p\n", [Ev]),
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

create_example(Parent, Example) ->
    Frame = wxFrame:new(Parent, ?wxID_ANY, Example,
			[{style, ?wxDEFAULT_FRAME_STYLE bor ?wxFRAME_FLOAT_ON_PARENT}]),
    wxFrame:center(Frame),
    Panel = wxPanel:new(Frame, []),
    Sizer =
	case Example of
  	    "Proportional Resize With Alignments" ->
		proportional_resize_with_alignments(Panel);
	    "Grid With Alignment" ->
		grid_with_alignment(Panel);
	    "Flexible Grid" ->
		flexible_grid(Panel);
	    "More Grid Features" ->
		more_grid_features(Panel);
	    "Simple Grid" ->
		simple_grid(Panel);
	    "Border In A Box" ->
		border_in_a_box(Panel);
	    "Boxes Inside A Border" ->
		boxes_inside_a_border(Panel);
	    "Box In Box" ->
		box_in_box(Panel);
	    "East And West Border" ->
		east_and_west_border(Panel);
	    "North And South Border" ->
		north_and_south_border(Panel);
	    "Simple Border" ->
		simple_border(Panel);
	    "Centering In Avalible Space" ->
		centering_in_avalible_space(Panel);
	    "Spacer" ->
		spacer(Panel);
	    "Edge Affinity" ->
		edge_affinity(Panel);
	    "Weighting Factor" ->
		weighting_factor(Panel);
	    "More Than One Strechable" ->
		more_than_one_strechable(Panel);
	    "Add A Strechable" ->
		add_a_strechable(Panel);
	    "Vertical Example" ->
		vertical(Panel);
	    "Horizontal Example" ->
		horizontal(Panel)
	end,
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Panel),
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "Resize window to see how the sizers respond.."),
    wxFrame:fit(Frame),
    wxFrame:show(Frame).

create_box(Parent) ->
    Win = wxWindow:new(Parent, ?wxID_ANY, [{style, ?wxBORDER_SIMPLE},
					   {size, {50,25}}]),
    wxWindow:setBackgroundColour(Win, ?wxWHITE),
    Win.


vertical(Parent) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    Sizer.
    

horizontal(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    Sizer.

add_a_strechable(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    Sizer.

more_than_one_strechable(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    Sizer.

weighting_factor(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 3}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    Sizer.

edge_affinity(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxALIGN_TOP}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxALIGN_BOTTOM}]),
    Sizer.

spacer(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, 60,20,              [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    Sizer.

centering_in_avalible_space(Parent) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, 0,0,                [{proportion, 1}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxALIGN_CENTER}]),
    wxSizer:add(Sizer, 0,0,                [{proportion, 1}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    Sizer.

simple_border(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    Win = create_box(Parent),
    wxWindow:setSize(Win, 80,80),
    wxSizer:add(Sizer, Win, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL},
			     {border, 15}]),
    Sizer.

east_and_west_border(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    Win = create_box(Parent),
    wxWindow:setSize(Win, 80,80),
    wxSizer:add(Sizer, Win, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxEAST bor ?wxWEST},
			     {border, 15}]),
    Sizer.

north_and_south_border(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    Win = create_box(Parent),
    wxWindow:setSize(Win, 80,80),
    wxSizer:add(Sizer, Win, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxNORTH bor ?wxSOUTH},
			     {border, 15}]),
    Sizer.


box_in_box(Parent) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),

    Sizer2 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),

    Sizer3 = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    
    wxSizer:add(Sizer2, Sizer3, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, Sizer2, [{proportion, 1}, {flag, ?wxEXPAND}]),
    
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    Sizer.

boxes_inside_a_border(Parent) ->
    Border = wxBoxSizer:new(?wxHORIZONTAL),
    Sizer = add_a_strechable(Parent),
    wxSizer:add(Border, Sizer, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL},
				{border, 15}]),
    Border.

border_in_a_box(Parent) ->
    InsideBox = wxBoxSizer:new(?wxHORIZONTAL),

    Sizer2 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),

    wxSizer:add(InsideBox, Sizer2, [{proportion, 0}, {flag, ?wxEXPAND}]),

    Border = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Border, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}]),
    wxSizer:add(InsideBox, Border, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL},
				    {border, 20}]),
    

    Sizer3 = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer3, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),
    
    wxSizer:add(InsideBox, Sizer3, [{proportion, 1}, {flag, ?wxEXPAND}]),

    OutsideBox = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(OutsideBox, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(OutsideBox, InsideBox, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(OutsideBox, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    OutsideBox.
    
simple_grid(Parent) ->
    GridSizer = wxGridSizer:new(3, 3, 2, 2),

    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    GridSizer.

more_grid_features(Parent) ->
    GridSizer = wxGridSizer:new(3, 3, 1, 1), % rows, cols, vgap, hgap

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, create_box(Parent), [{proportion, 1}, {flag, ?wxEXPAND}]),

    GridSizer2 = wxGridSizer:new(2, 2, 4, 4),
    wxSizer:add(GridSizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer2, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),

    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxALIGN_RIGHT bor ?wxALIGN_BOTTOM}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxALIGN_LEFT bor ?wxALIGN_BOTTOM}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxALIGN_CENTER}]),
    wxSizer:add(GridSizer, Sizer,              [{proportion, 0}, {flag, ?wxEXPAND bor ?wxALL},
						{border, 10}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(GridSizer, GridSizer2,         [{proportion, 0}, {flag, ?wxEXPAND bor ?wxALL},
						{border, 4}]),
    GridSizer.


flexible_grid(Parent) ->
    FlexGridSizer = wxFlexGridSizer:new(3, 3, 2, 2), % rows, cols, vgap, hgap
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, 175, 50, []),
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(FlexGridSizer, create_box(Parent), [{proportion, 0}, {flag, ?wxEXPAND}]),

    wxFlexGridSizer:addGrowableRow(FlexGridSizer, 0),
    wxFlexGridSizer:addGrowableRow(FlexGridSizer, 2),
    wxFlexGridSizer:addGrowableCol(FlexGridSizer, 1),
    FlexGridSizer.

grid_with_alignment(Parent) ->
    GridSizer = wxGridSizer:new(3, 3, 2, 2), % rows, cols, vgap, hgap

    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_LEFT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_CENTER_HORIZONTAL}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_RIGHT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_CENTER}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_LEFT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_CENTER_HORIZONTAL}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_RIGHT}]),
    GridSizer.

proportional_resize_with_alignments(Parent) ->    
    GridSizer = wxGridSizer:new(3, 3, 2, 2), % rows, cols, vgap, hgap

    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_TOP bor ?wxALIGN_LEFT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_TOP bor ?wxALIGN_CENTER_HORIZONTAL}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_TOP bor ?wxALIGN_RIGHT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_CENTER}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_BOTTOM bor ?wxALIGN_LEFT}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_BOTTOM bor ?wxALIGN_CENTER_HORIZONTAL}]),
    wxSizer:add(GridSizer, create_box(Parent),
		[{proportion, 0}, {flag, ?wxSHAPED bor ?wxALIGN_BOTTOM bor ?wxALIGN_RIGHT}]),
    GridSizer.

