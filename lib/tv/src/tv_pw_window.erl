%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%%*********************************************************************
%%% 
%%%   Description:      Part of the pw component controlling the graphics.
%%%
%%%*********************************************************************


-module(tv_pw_window).



-export([create_window/2, 
	 resize_window/3, 
	 create_menubar/2, 
	 create_menu/2]).




-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_pw_int_def.hrl").



-define(DEFAULT_BG_COLOR, {217, 217, 217}).




%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      create_menu.
%%
%% Return Value:  Identifier to the menu created.
%%
%% Description:   Creates a menu in the window.
%%
%% Parameters:    Win:  ID of parent window.
%%======================================================================


create_menu(Msg, ProcVars) ->
    MenuP        = ProcVars#process_variables.menu_params,
    MenubarId    = MenuP#menu_params.menubar_id,
    ShortcutList = MenuP#menu_params.shortcuts,
    
    #pw_create_menu{menutitle     = MenuTitle, 
		    title_acc_pos = TitleAccPos,
		    menulist      = MenuList}  = Msg,

       % Create the menubutton!
    Label = def_or_param(MenuTitle, "NoName"),
    Mbutt = gs:create(menubutton, MenubarId, [{bg, ?DEFAULT_BG_COLOR},
					      {fg, {178, 34, 34}},  % firebrick
					      %  {font, {helvetica, bold, 14}},
					      {label, {text, Label}},
					      {underline, TitleAccPos}
					     ]),

       % Create the actual menu!
    Menu = gs:create(menu, Mbutt, [{bg, ?DEFAULT_BG_COLOR},
				   {fg, {178, 34, 34}}
				  ]), 

    NewMenuP = MenuP#menu_params{shortcuts = ShortcutList ++ create_menulist(MenuList, Menu)},

    ProcVars#process_variables{menu_params = NewMenuP}.



    



create_menubar(WinP, MenuP) ->
    WindowId  = WinP#window_params.window_id,
    MenubarId = gs:create(menubar, WindowId, [{bg, ?DEFAULT_BG_COLOR}
					     ]),
    Mbutt = gs:create(menubutton, MenubarId, [{bg, ?DEFAULT_BG_COLOR},
					      {fg, {178, 34, 34}},  % firebrick
					      %  {font, {helvetica, bold, 14}},
					      {label, {text, " Help "}},
					      {underline, 1},
					      {side, right}
					     ]),

       % Create the actual menu!
    Menu = gs:create(menu, Mbutt, [{bg, ?DEFAULT_BG_COLOR},
				   {fg, {178, 34, 34}}
				  ]), 
    MenuP#menu_params{menubar_id = MenubarId,
		      shortcuts  = create_menulist([{" Help ", normal, help_button, 1, h},
						    separator,
						    {" OTP Documentation ",normal,otp_help_button,1,no_char}], 
						   Menu) ++ [{x,exit_button}, {'X',exit_button}]
		     }.





create_window(Msg, WinP) ->
    #pw_deblock{win_title      = Title, 
		win_width      = Width, 
		win_height     = Height,
		min_win_width  = MinWidth} = Msg,

    
    S = gs:start(),
    WindowTitle     = def_or_param(Title, "NoName"),
    WindowMinWidth  = def_or_param(MinWidth, ?DEFAULT_MIN_WINDOW_WIDTH),
    WindowMinHeight = def_or_param(MinWidth, ?DEFAULT_MIN_WINDOW_HEIGHT),
    WindowWidth     = ?COMM_FUNC_FILE:max(def_or_param(Width, 
						       ?DEFAULT_WINDOW_WIDTH), 
					  WindowMinWidth),
    WindowHeight    = ?COMM_FUNC_FILE:max(def_or_param(Height, 
						       ?DEFAULT_WINDOW_HEIGHT),
					  WindowMinHeight),
    
    
    WindowId = gs:create(window, S, [{title, WindowTitle},
				     {width, WindowWidth},
				     {height, WindowHeight},
				     {bg, ?DEFAULT_BG_COLOR},
				     {configure, true},
				     {destroy, true},
				     {keypress, true},
				     {cursor, arrow}
				    ]),
    
    WinP#window_params{window_id         = WindowId,
		       window_title      = WindowTitle,
		       window_width      = WindowWidth,
		       window_height     = WindowHeight,
		       min_window_width  = WindowMinWidth,
		       min_window_height = WindowMinHeight
		      }.







resize_window(WindowId, NewWidth, NewHeight) ->
    gs:config(WindowId, [{width, NewWidth},
			 {height, NewHeight}
			]).




%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************



create_menulist([], _Menu) ->
    [];
create_menulist(List, Menu) ->
    MaxLength = get_length_of_longest_menu_text(List, 0),
    create_menulist(List, Menu, MaxLength).




create_menulist([], _Menu, _MaxLength) ->
    [];
create_menulist([{Text, Type, Data, AccCharPos, ShortcutChar} | Rest], Menu, MaxLength) ->
    ShortcutCapitalChar = 
	if
	    ShortcutChar =:= no_char ->
		no_char;
	    true ->
		CharAsciiValue   = lists:nth(1, atom_to_list(ShortcutChar)),
		CapitalCharValue = CharAsciiValue - ($a - $A),
		list_to_atom([CapitalCharValue])
	end,
    
    FinalText = if 
		    ShortcutChar =:= no_char ->
			Text;
		    true ->
			Text ++ lists:duplicate(MaxLength - length(Text), " ") ++ 
			    "   Ctrl+" ++ atom_to_list(ShortcutCapitalChar) ++ " "
		end,
    TypeAndSel = 
	case Type of
	    normal ->
		[{itemtype, normal}];
	    {radio, Selected, Group} ->
		[{itemtype, radio},
		 {select, Selected},
		 {group, Group}];
	    {check, Selected} ->
		[{itemtype, check},
		 {select, Selected}]
	end,
    gs:menuitem(Data, Menu, [{bg, ?DEFAULT_BG_COLOR},
			     {fg, {178, 34, 34}},
			     {label, {text, FinalText}},
			     {underline, AccCharPos},
			     {data, Data} |
			     TypeAndSel
			    ]),
    [{ShortcutChar, Data}, {ShortcutCapitalChar, Data} | create_menulist(Rest, Menu, MaxLength)];
create_menulist([separator | Rest], Menu, MaxLength) ->
    gs:create(menuitem, Menu, [{itemtype, separator}
			      ]),
    create_menulist(Rest, Menu, MaxLength).


    
    
    


get_length_of_longest_menu_text([], MaxLength) ->
    MaxLength;
get_length_of_longest_menu_text([{Text, _Type, _Data, _APos, _SChar} | Rest], CurrMax) ->
    L = length(Text),
    if 
	L > CurrMax ->
	    get_length_of_longest_menu_text(Rest, L);
	true ->
	    get_length_of_longest_menu_text(Rest, CurrMax)
    end;
get_length_of_longest_menu_text([separator | Rest], CurrMax) ->
    get_length_of_longest_menu_text(Rest, CurrMax).





def_or_param(undefined, DefaultValue) ->
    DefaultValue;
def_or_param(Param, _Default) ->
    Param.










