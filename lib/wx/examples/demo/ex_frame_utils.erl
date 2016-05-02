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

-module(ex_frame_utils).

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
				 [{label, "Utilities"}]),

    Labels = [{"Open window",1}, {"Open wxMiniFrame",2}, {"Open erlang.org",3}],
    Buttons = [wxButton:new(Panel, Id, [{label, L}])|| {L,Id} <- Labels],

    %% Add to sizers
    [wxSizer:add(Sizer, Button) || Button <- Buttons],
    wxPanel:connect(Panel, command_button_clicked),
    wxSizer:add(MainSizer, Sizer),
    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{id = Id,
		 event = #wxCommand{type = command_menu_selected}},
	     State = #state{}) ->
    case Id of
	?wxID_NEW -> demo:format(State#state.config, "New\n", []);
	?wxID_OPEN -> demo:format(State#state.config, "Open\n", []);
	?wxID_COPY -> demo:format(State#state.config, "Copy\n", []);
	?wxID_PASTE -> demo:format(State#state.config, "Paste\n", []);
	?wxID_HELP ->
	    wx_misc:launchDefaultBrowser("http://erlang.org/doc/apps/wx/part_frame.html");
	_ -> ignore
    end,
    {noreply, State};
handle_event(#wx{id = Id,
		event = #wxCommand{type = command_button_clicked}},
	     State = #state{}) ->
    case Id of
	1 -> new_win(State#state.parent);
	2 -> new_mini_frame(State#state.parent);
	3 -> wx_misc:launchDefaultBrowser("http://erlang.org/");
	_ -> ignore
    end,
    {noreply, State};
handle_event(#wx{userData = StatusBar,
		 event = #wxMouse{type = motion, x = X, y = Y}},
	     State) ->
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Mouse position: ~p", [{X,Y}]),
			      [{number, 1}]),
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

new_win(Panel) ->
    Frame = wxFrame:new(Panel, ?wxID_ANY, "Utilities", [{style,
							 ?wxCAPTION bor
							 ?wxCLIP_CHILDREN bor
							 ?wxCLOSE_BOX bor
							 ?wxFRAME_FLOAT_ON_PARENT bor
							 %%?wxFRAME_NO_TASKBAR bor
							 ?wxMAXIMIZE_BOX bor
							 ?wxMINIMIZE_BOX bor
							 ?wxRESIZE_BORDER bor
							 %%?wxSTAY_ON_TOP bor
							 ?wxSYSTEM_MENU
							}]),

    %% Setup wxMenuBar
    MB = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Help    = wxMenu:new([]),
    Mbar    = wxMenu:new([]),
    wxMenu:append(File, ?wxID_NEW, "New"),
    wxMenu:append(File, ?wxID_OPEN, "Open"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    wxMenu:append(Help, ?wxID_HELP, "Help"), 
    wxMenu:append(Mbar, ?wxID_ANY, "Test item 1"), 
    wxMenu:append(Mbar, ?wxID_ANY, "Test item 2"), 
    wxMenu:append(Mbar, ?wxID_ANY, "Test item 3"), 

    wxMenuBar:append(MB, File, "&File"),
    wxMenuBar:append(MB, Help, "&Help"),
    wxMenuBar:append(MB, Mbar, "This is a menu bar"),
    wxFrame:setMenuBar(Frame,MB),

    %% Setup wxStatusBar
    StatusBar = wxFrame:createStatusBar(Frame, []),
    wxStatusBar:setFieldsCount(StatusBar, 2),
    wxStatusBar:setStatusText(StatusBar, "This is a status bar", [{number, 0}]),

    %% Setup wxToolBar
    ToolBar = wxFrame:createToolBar(Frame, []),
    wxToolBar:addTool(ToolBar, ?wxID_NEW, "New", wxArtProvider:getBitmap("wxART_NEW"),
		      [{shortHelp, "New"}]),
    wxToolBar:setToolLongHelp(ToolBar, ?wxID_NEW, "This is long help for 'New'"),
    wxToolBar:addTool(ToolBar, ?wxID_OPEN, "Open", wxArtProvider:getBitmap("wxART_FILE_OPEN"),
		      [{shortHelp, "Open"}]),
    wxToolBar:setToolLongHelp(ToolBar, ?wxID_OPEN, "This is long help for 'Open'"),
    wxToolBar:addSeparator(ToolBar),
    wxToolBar:addTool(ToolBar, ?wxID_COPY, "Copy", wxArtProvider:getBitmap("wxART_COPY"),
		      [{shortHelp, "Copy"}]),
    wxToolBar:setToolLongHelp(ToolBar, ?wxID_COPY, "This is long help for 'Copy'"),
    wxToolBar:addTool(ToolBar, ?wxID_PASTE, "Paste", wxArtProvider:getBitmap("wxART_PASTE"),
		      [{shortHelp, "Paste"}]),
    wxToolBar:setToolLongHelp(ToolBar, ?wxID_PASTE, "This is long help for 'Paste'"),

    wxToolBar:addControl(ToolBar,wxStaticText:new(ToolBar, 5, "This is a tool bar")),    


    wxToolBar:realize(ToolBar),
    wxFrame:setToolBar(Frame,ToolBar),

    wxFrame:connect(Frame, motion, [{userData, StatusBar}]),
    wxFrame:connect(Frame, command_menu_selected, []),
    wxFrame:center(Frame),
    wxFrame:show(Frame).


new_mini_frame(Parent) ->
    MiniFrame = wxMiniFrame:new(Parent, ?wxID_ANY, "wxMiniFrame", [{style,
								    ?wxDEFAULT_FRAME_STYLE bor
								    ?wxFRAME_FLOAT_ON_PARENT}]),
    Panel = wxPanel:new(MiniFrame, []),

    Text = "This is a wxMiniFrame",

    wxStaticText:new(Panel, ?wxID_ANY, Text, [{style, ?wxALIGN_CENTER}]),
    wxMiniFrame:setSize(MiniFrame, {200,200}),
    wxMiniFrame:center(MiniFrame),
    wxMiniFrame:show(MiniFrame).
    

