%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : minimal.erl
%%% Author  : Matthew Harrison <harryhuk at users.sourceforge.net>
%%% Description : Minimal example of a wxerlang application
%%%
%%% Created :  18 Sep 2008 by  Matthew Harrison <harryhuk at users.sourceforge.net>
%%%-------------------------------------------------------------------
-module(minimal).

-include_lib("wx/include/wx.hrl").

-export([start/0]).
-compile(export_all).

start() ->
    Wx = wx:new(),
    Frame = wx:batch(fun() -> create_window(Wx) end),
    wxWindow:show(Frame),
    loop(Frame),
    wx:destroy(),
    ok.

create_window(Wx) ->
    Frame = wxFrame:new(Wx, -1, "Minimal wxErlang App", [{size, {600,400}}]),

    Path = filename:dirname(code:which(?MODULE)),    
    wxFrame:setIcon(Frame,  wxIcon:new(filename:join(Path,"sample.xpm"))),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    MenuBar  = wxMenuBar:new(),
    FileM    = wxMenu:new([]),
    HelpM    = wxMenu:new([]),

    % unlike wxwidgets the stock menu items still need text to be given, 
    % although help text does appear
    _QuitMenuItem  = wxMenu:append(FileM, ?wxID_EXIT, "&Quit"),
    % Note the keybord accelerator
    _AboutMenuItem = wxMenu:append(HelpM, ?wxID_ABOUT, "&About...\tF1"),

    wxMenu:appendSeparator(HelpM),    
    ContentsMenuItem = wxMenu:append(HelpM, ?wxID_HELP_CONTENTS, "&Contents"),
    wxMenuItem:enable(ContentsMenuItem, [{enable, false}]),

    ok = wxFrame:connect(Frame, command_menu_selected), 

    wxMenuBar:append(MenuBar, FileM, "&File"),
    wxMenuBar:append(MenuBar, HelpM, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),

    ok = wxFrame:setStatusText(Frame, "Welcome to wxErlang!",[]),
    Frame.

loop(Frame) ->
    receive 
  	#wx{event=#wxClose{}} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxFrame:destroy(Frame),
  	    ok;
	#wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    wxWindow:destroy(Frame),
	    ok;
	#wx{id=?wxID_ABOUT, event=#wxCommand{type=command_menu_selected}} ->
	    io:format("Got about ~n", []),
	    dialog(?wxID_ABOUT, Frame),
	    loop(Frame);
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop(Frame)
    after 1000 ->
	io:fwrite("."),
	loop(Frame)
    end.

dialog(?wxID_ABOUT,  Frame) ->
    Str = string:join(["Welcome to wxErlang.", 
		       "This is the minimal wxErlang sample\n",
		       "running under ",
		       wx_misc:getOsDescription(),
		       "."], 
		      ""),
    MD = wxMessageDialog:new(Frame,
   			     Str,
   			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
   			      {caption, "About wxErlang minimal sample"}]),

    wxDialog:showModal(MD),
    wxDialog:destroy(MD).


