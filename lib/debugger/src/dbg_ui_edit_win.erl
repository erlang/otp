%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(dbg_ui_edit_win).

%% External exports
-export([create_win/5, get_window/1,
	 handle_event/2]).

-record(winInfo, {window,   % gsobj()
		  entry,    % gsobj()
		  button,   % gsobj()
		  type      % atom()
		 }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% create_win(GS, Pos, Title, Prompt, {Type, Value}) -> #winInfo{}
%%   GS = graphics system identifier
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Title = string()
%%   Prompt = atom()
%%   Type = term | atom | float | integer | string
%%   Value = term()
%%--------------------------------------------------------------------
create_win(GS, {X, Y}, Title, Prompt, {Type, Value}) ->
    Pad=8,

    Font = dbg_ui_win:font(normal),

    %% Window
    Win = gs:window(GS, [{title, Title}, {x, X}, {y, Y},
			 {destroy, true}]),

    %% Label
    {Wlbl, Hlbl} = dbg_ui_win:min_size([Prompt], 50, 30),
    gs:label(Win, [{x, Pad}, {y, Pad}, {width, Wlbl}, {height, Hlbl},
		   {align, e}, {label, {text, Prompt}}, {font, Font}]),


    %% Entry
    {Went, _Hent} = dbg_ui_win:min_size([Value], 100, 20),
    Ent = gs:entry(Win, [{x, Pad+Wlbl}, {y, Pad},
			 {width, Went}, {height, Hlbl},
			 {text, Value},
			 {keypress, true}]),

    %% Ok and Cancel buttons
    W = Pad + Wlbl + Went + Pad,
    {Wbtn, Hbtn} = dbg_ui_win:min_size(["Cancel"], 70, 30),
    Ybtn = Pad + Hlbl + Pad,
    Btn = gs:button(Win, [{x, Pad}, {y, Ybtn},
			  {width, Wbtn}, {height, Hbtn},
			  {label, {text,"Ok"}}, {font, Font}]),
    gs:button(Win, [{x, W-Pad-Wbtn}, {y, Ybtn},
		    {width, Wbtn}, {height, Hbtn},
		    {label, {text,"Cancel"}}, {font, Font}]),

    H = Ybtn + Hbtn + Pad,
    gs:config(Win, [{width, W}, {height, H}, {map, true}]),

    #winInfo{window=Win, entry=Ent, button=Btn, type=Type}.

%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = gsobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%% GSEvent = {gs, Id, Event, Data, Arg}
%% WinInfo = #winInfo{}
%% Command = ignore
%%         | stopped
%%         | {edit, Value}
%%--------------------------------------------------------------------
handle_event({gs, _Id, destroy, _Data, _Arg}, _WinInfo) ->
    stopped;
handle_event({gs, Id, keypress, Data, ['Return'|_]}, WinInfo) ->
    gs:config(WinInfo#winInfo.button, flash),
    handle_event({gs, Id, click, Data, ["Ok"]}, WinInfo);
handle_event({gs, _Id, click, _Data, ["Ok"|_]}, WinInfo) ->
    Ent = WinInfo#winInfo.entry,
    Str = gs:read(Ent, text),
    Type = WinInfo#winInfo.type,
    case erl_scan:string(Str) of
	{ok, Tokens, _EndLine} when Type==term ->
	    case erl_parse:parse_term(Tokens++[{dot, 1}]) of
		{ok, Value} -> {edit, Value};
		_Error -> ignore
	    end;
	{ok, [{Type, _Line, Value}], _EndLine} when Type/=term ->
	    {edit, Value};
	_ ->
	    ignore
    end;
handle_event({gs, _Id, click, _Data, ["Cancel"|_]}, _WinInfo) ->
    stopped;
handle_event(_GSEvent, _WinInfo) ->
    ignore.
