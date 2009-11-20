%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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

%%
-module(menu_demo).

-compile(export_all).

start() -> spawn(menu_demo, init, []).

init() ->
    I=gs:start(),
    Win=gs:window(I, [{title,"menu"},{width,200},{height,100}, {map, true}]),
    Bar = gs:create(menubar, Win, []),
    Fmb = gs:create(menubutton, Bar, [{label, {text, "File"}}]),
    Emb = gs:create(menubutton, Bar, [{label, {text, "Edit"}}]),
    Hmb = gs:create(menubutton, Bar, [{label, {text, "Help"}}, {side, right}]),
    Fmnu = gs:create(menu, Fmb, []),
    Emnu = gs:create(menu, Emb, []),
    Hmnu = gs:create(menu, Hmb, []),
    gs:create(menuitem, load, Fmnu, [{label,{text, "Load"}}]),
    gs:create(menuitem, save, Fmnu, [{label,{text, "Save"}}]),
    Exit = gs:create(menuitem, Fmnu, [{label,{text, "Exit"}}]),
    Color=gs:create(menuitem,Emnu,[{label,{text,"Color"}},{itemtype,cascade}]),
    Cmnu = gs:create(menu, Color, [{disabledfg,gray}]),
    gs:create(menuitem, Cmnu, [{label, {text,"Red"}}, {data, {new_color, red}},
			       {fg,red}, {itemtype,radio},{group,gr1}]),
    gs:create(menuitem, Cmnu, [{label, {text,"Blue"}},{data, {new_color, blue}},
			       {fg,blue}, {itemtype,radio},{group,gr1}]),
    gs:create(menuitem,Cmnu,[{label, {text,"Black"}},{data, {new_color, black}},
			      {fg,black}, {itemtype,radio},{group,gr1}]),
    Y = gs:create(menuitem, Hmnu, [{label, {text,"You"}}, {itemtype, check}]),
    M = gs:create(menuitem,me,Hmnu,[{label,{text,"Me"}},{itemtype,check}]),
    gs:create(menuitem, Hmnu, [{label, {text, "Other"}}, {itemtype, check},
			       {enable,false},{click,false}]),
    gs:create(menuitem,doit,Hmnu,[{label,{text,"Doit!"}},{data,{doit,Y,M}}]),
    loop(Exit, Win).

loop(Exit, Win) ->
    receive
	{gs, save, click, _Data, [_Txt, _Index | _Rest]} ->
	    io:format("Save~n");
	{gs, load, click, _Data, [_Txt, _Index | _Rest]} ->
	    io:format("Load~n");
	{gs, Exit, click, _Data, [_Txt, _Index | _Rest]} ->
	    io:format("Exit~n"),
	    exit(normal);
	{gs, _MnuItem, click, {new_color, Color}, Args} ->
	    io:format("Change color to ~w. Args:~p~n", [Color, Args]),
	    gs:config(Win, [{bg, Color}]);
	{gs, doit, click, {doit, YouId, MeId}, Args} ->
	    HelpMe = gs:read(MeId, select),
	    HelpYou = gs:read(YouId, select),
	    io:format("Doit. HelpMe:~w, HelpYou:~w, Args:~p~n",
		      [HelpMe, HelpYou, Args]);
	Other -> io:format("Other:~p~n",[Other])
    end,
    loop(Exit, Win).
