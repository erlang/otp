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

-module(ex_dialogs).

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
	  dialogs,
	  choices
	}).


start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),
    Panel = wxScrolledWindow:new(Parent, []),
    wxScrolledWindow:setScrollRate(Panel, 5, 5),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "Dialogs"}]),

    Buttons =
	[wxButton:new(Panel, 1,  [{label, "wxDirDialog"}]),
	 wxButton:new(Panel, 2,  [{label, "wxFileDialog"}]),
	 wxButton:new(Panel, 3,  [{label, "wxColourDialog"}]),
	 wxButton:new(Panel, 4,  [{label, "wxMessageDialog"}]),
	 wxButton:new(Panel, 5,  [{label, "wxTextEntryDialog"}]),
	 wxButton:new(Panel, 6,  [{label, "wxSingleChoiceDialog"}]),
	 wxButton:new(Panel, 7,  [{label, "wxMultiChoiceDialog"}]),
	 wxButton:new(Panel, 10, [{label, "wxFontDialog"}])],
    
    Choices = ["Orange","Banana", "Apple", "Lemon", "Pear",
	       "Carrot", "Potato", "Peach", "Tomato", "Grape",
	       "Pineapple", "Blueberry"],
    Dialogs = [{wxDirDialog, [Panel, []]},
	       {wxFileDialog, [Panel, []]},
	       {wxColourDialog, [Panel, []]},
	       {wxSingleChoiceDialog, [Panel, "wxSingleChoiceDialog\n"
				       "Feel free to pick one of "
				       "these items !", "Caption",
				       Choices]},
	       {wxMultiChoiceDialog, [Panel, "wxMultiChoiceDialog\n"
				      "Feel free to pick one of "
				      "these items !", "Caption",
				      Choices]},
	       {wxMessageDialog, [Panel, "This is a wxMessageDialog !"]},
	       {wxTextEntryDialog, [Panel, "This is a wxTextEntryDialog !",
				    [{value, "Erlang is the best !"}]]},
	       {wxFontDialog, [get_parent(Parent), wxFontData:new()]}],

    %% Add to sizers
    Fun = fun(Button) ->
		  Label = list_to_atom(wxButton:getLabel(Button)),
		  wxSizer:add(Sizer, Button, [{border, 4}, {flag, ?wxALL bor ?wxEXPAND}]),
		  wxButton:connect(Button, command_button_clicked, [{userData, Label}])
	  end,
    wx:foreach(Fun, Buttons),

    wxSizer:add(MainSizer, Sizer),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		   dialogs = Dialogs, choices = Choices}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxCommand{type = command_button_clicked},
		 userData = Module},
	     State = #state{dialogs = Dialogs, choices = Choices}) ->
    Args = proplists:get_value(Module, Dialogs),
    Dialog = apply(Module, new, Args),

    case Module:showModal(Dialog) of
	?wxID_OK ->
	    case Module of
		wxColourDialog ->
		    Colour = wxColourData:getColour(Module:getColourData(Dialog)),
		    demo:format(State#state.config, "Colour: ~p\n", [Colour]);
	        wxSingleChoiceDialog ->
		    Selection = Module:getStringSelection(Dialog),
		    demo:format(State#state.config, "Selection: ~p\n", [Selection]);
		wxTextEntryDialog ->
		    Value = Module:getValue(Dialog),
		    demo:format(State#state.config, "Value: ~p\n", [Value]);
		wxFontDialog ->
		    Font = wxFontData:getChosenFont(Module:getFontData(Dialog)),
		    FontDesc = wxFont:getNativeFontInfoUserDesc(Font),
		    demo:format(State#state.config,
				"Font: ~p\n",
				[FontDesc]);
		wxFileDialog ->
		    demo:format(State#state.config, "File path: ~p\n", [Module:getPath(Dialog)]);
		wxDirDialog ->
		    demo:format(State#state.config, "Dir path: ~p\n", [Module:getPath(Dialog)]);
		wxMultiChoiceDialog ->
		    Selected = [lists:nth(Num+1, Choices) ||
				   Num <- Module:getSelections(Dialog)],
		    demo:format(State#state.config, "Selections: ~p\n", [Selected]);
		wxMessageDialog ->
		    ok
	    end;
	?wxID_CANCEL -> cancel;
	Any -> io:format("Any: ~p\n", [Any])
    end,
    Module:destroy(Dialog),
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

get_parent(Window) ->
    Parent = wxWindow:getParent(Window),
    case wx:is_null(Parent) of
	false -> get_parent(Parent);
	true ->  Window
    end.
