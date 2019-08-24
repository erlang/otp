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

-module(ex_pickers).

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
    DirPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "wxDirPickerCtrl"}]),
    FilePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "wxFilePickerCtrl"}]),
    FontPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "wxFontPickerCtrl"}]),
    DatePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "wxDatePickerCtrl"}]),
    ColourPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "wxColourPickerCtrl"}]),

    DirPicker = wxDirPickerCtrl:new(Panel, 1, [{path, "/"}]),
    FilePicker = wxFilePickerCtrl:new(Panel, 2, [{path, "/"}]),
    FontPicker = wxFontPickerCtrl:new(Panel, 3, []),
    DatePicker = wxDatePickerCtrl:new(Panel, 4, []),
    ColourPicker = wxColourPickerCtrl:new(Panel, 5, []),

    wxColourPickerCtrl:connect(ColourPicker, command_colourpicker_changed, []),
    wxDirPickerCtrl:connect(DirPicker, command_dirpicker_changed, []),
    wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
    wxFontPickerCtrl:connect(FontPicker, command_fontpicker_changed, []),
    wxDatePickerCtrl:connect(DatePicker, date_changed, []),

    %% Add to sizers
    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],
    wxSizer:add(DirPickerSizer, DirPicker, PickerOptions),
    wxSizer:add(FilePickerSizer, FilePicker, PickerOptions),
    wxSizer:add(FontPickerSizer, FontPicker, PickerOptions),
    wxSizer:add(DatePickerSizer, DatePicker, PickerOptions),
    wxSizer:add(ColourPickerSizer, ColourPicker, PickerOptions),

    SizerOptions  = [{flag, ?wxEXPAND}],
    wxSizer:add(MainSizer, DirPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, FilePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, FontPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, DatePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, ColourPickerSizer, SizerOptions),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxFontPicker{font = Font}}, State = #state{}) ->
    demo:format(State#state.config, "Font changed to ~p.\n", [Font]),
    {noreply, State};
handle_event(#wx{event = #wxColourPicker{colour = Colour}}, State = #state{}) ->
    demo:format(State#state.config, "Colour changed to ~p.\n", [Colour]),
    {noreply, State};
handle_event(#wx{event = #wxFileDirPicker{type = command_filepicker_changed,
					  path = Path}},
	     State = #state{}) ->
    demo:format(State#state.config, "Filepicker changed to ~p.\n", [Path]),
    {noreply, State};
handle_event(#wx{event = #wxFileDirPicker{type = command_dirpicker_changed,
					  path = Path}},
	     State = #state{}) ->
    demo:format(State#state.config, "Dirpicker changed to ~p.\n", [Path]),
    {noreply, State};
handle_event(#wx{event = #wxDate{date = Date}},
	     State = #state{}) ->
    demo:format(State#state.config, "Datepicker changed to ~p.\n", [Date]),
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

