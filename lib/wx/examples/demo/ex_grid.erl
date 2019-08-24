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

-module(ex_grid).

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
	  grid
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
				 [{label, "wxGrid"}]),

    Grid = create_grid(Panel),

    %% Add to sizers
    Options = [{flag, ?wxEXPAND}, {proportion, 1}],

    wxSizer:add(Sizer, Grid, Options),
    wxSizer:add(MainSizer, Sizer, Options),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		  grid = Grid}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxGrid{type = grid_cell_change,
				 row = Row, col = Col}},
	     State = #state{}) ->
    Val = wxGrid:getCellValue(State#state.grid, Row, Col),
    demo:format(State#state.config, "Cell {~p,~p} changed to ~p.\n",
		[Row,Col,Val]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
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

create_grid(Panel) ->
    %% Create the grid with 100 * 5 cells
    Grid = wxGrid:new(Panel, 2, []),
    wxGrid:createGrid(Grid, 100, 5),

    Font = wxFont:new(16, ?wxFONTFAMILY_SWISS,
		      ?wxFONTSTYLE_NORMAL,
		      ?wxFONTWEIGHT_NORMAL, []),
    %% Fun to set the values and flags of the cells
    Fun =
	fun(Row) ->
		wxGrid:setCellValue(Grid, Row, 0, "Editable"),
		wxGrid:setCellValue(Grid, Row, 1, "Editable"),
		wxGrid:setCellValue(Grid, Row, 2, "Editable"),
		wxGrid:setCellValue(Grid, Row, 3, "Read only"),
		wxGrid:setCellTextColour(Grid, Row, 3, ?wxWHITE),
		wxGrid:setReadOnly(Grid, Row, 3, [{isReadOnly,true}]),
		wxGrid:setCellValue(Grid, Row, 4, "Editable"),
		case Row rem 4 of
		    0 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxRED);
		    1 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxGREEN),
			 wxGrid:setCellTextColour(Grid, Row, 2, {255,215,0,255});
		    2 -> wxGrid:setCellBackgroundColour(Grid, Row, 3, ?wxBLUE);
		    _ -> wxGrid:setCellBackgroundColour(Grid, Row, 1, ?wxCYAN),
			 wxGrid:setCellValue(Grid, Row, 1,
					     "Centered\nhorizontally"),
			 wxGrid:setCellAlignment(Grid, Row, 4,
						 0,?wxALIGN_CENTER),
			 wxGrid:setCellValue(Grid, Row, 4,
					     "Centered\nvertically"),
			 wxGrid:setCellAlignment(Grid, Row, 1,
						 ?wxALIGN_CENTER,0),
			 wxGrid:setCellTextColour(Grid, Row, 3, ?wxBLACK),
			 wxGrid:setCellAlignment(Grid, Row, 2,
						 ?wxALIGN_CENTER,
						 ?wxALIGN_CENTER),
			 wxGrid:setCellFont(Grid, Row, 0, Font),
			 wxGrid:setCellValue(Grid, Row, 2,
					     "Centered vertically\nand horizontally"),
			 wxGrid:setRowSize(Grid, Row, 80)
		end
	end,
    %% Apply the fun to each row
    wx:foreach(Fun, lists:seq(0,99)),
    wxGrid:setColSize(Grid, 2, 150),
    wxGrid:connect(Grid, grid_cell_change),
    Grid.


