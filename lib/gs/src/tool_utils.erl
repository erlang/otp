%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%

%%
-module(tool_utils).
-compile([{nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,create,3}},
          {nowarn_deprecated_function,{gs,destroy,1}},
          {nowarn_deprecated_function,{gs,read,2}}]).

-include_lib("kernel/include/file.hrl").

%%%---------------------------------------------------------------------
%%% Auxiliary functions to be used by the tools (internal module)
%%%---------------------------------------------------------------------

%% External exports
-export([open_help/2]).
-export([file_dialog/1]).
-export([notify/2, confirm/2, confirm_yesno/2, request/2]).

-record(state, {type,        % notify | confirm[_yesno] | request
		win,         % gsobj(), window
		entry,       % gsobj(), entry
		in_focus,    % 0 | 1 | undefined  Entry is in focus
		is_cursor,   % bool() | undefined  Cursor is over Entry
		buttons,     % [gsobj()], buttons
		highlighted  % int() highlighted buttone
	       }).


%%----------------------------------------------------------------------
%% open_help(Parent, File)
%%   Parent = gsobj()  (GS root object or parent window)
%%   File = string() | nofile
%% View the help file File, which can be an URL, an HTML file or a text
%% file.
%% This function is OS dependant.
%% Unix: Assumes Netscape is up & running, and use Netscape remote
%%   commands to display the file.
%% NT: If File is a file, use the NT command 'start' which will open the
%%   default tool for viewing the file.
%%   If File is an URL, try to view it using Netscape.exe which 
%%   requires that the path Netscape.exe must be in TBD.
%%   (TEMPORARY solution..., can be done better)
%%----------------------------------------------------------------------
open_help(Parent, nofile) ->
    notify(Parent, "Sorry, no help information exists");
open_help(Parent, File) ->
    case application:get_env(kernel, browser_cmd) of
	undefined ->
	    open_help_default(Parent, File);
	{ok, Cmd} when is_list(Cmd) ->
	    spawn(os, cmd, [Cmd ++ " " ++ File]);
	{ok, {M, F, A}} ->
	    apply(M, F, [File|A]);
	_Other ->
	    Str = ["Bad Kernel configuration parameter browser_cmd",
		   "Do not know how to display help file"],
	    notify(Parent, Str)
    end.

open_help_default(Parent, File) ->
    Cmd = case file_type(File) of

	      %% Local file
	      local ->
		  case os:type() of
		      {unix,Type} ->
                          case Type of
                               darwin -> "open " ++ File;
                               _Else -> "netscape -remote \"openURL(file:" ++ File ++ ")\""
			  end;
		      {win32,_AnyType} ->
			  "start " ++ filename:nativename(File);

		      _Other ->
			  unknown
		  end;

	      %% URL
	      remote ->
		  case os:type() of
		      {unix,Type} ->
                          case Type of
                               darwin -> "open " ++ File;
                               _Else -> "netscape -remote \"openURL(file:" ++ File ++ ")\""
			  end;
		      {win32,_AnyType} ->
			  "netscape.exe -h " ++
			      re:replace(File,"\\\\","/",[global,{return,list}]);
		      _Other ->
			  unknown
		  end;

	      Error -> % {error,Reason}
		  Error
	  end,

    if
	is_list(Cmd) ->
	    spawn(os, cmd, [Cmd]);
	Cmd==unknown ->
	    Str = ["Sorry, do not know how to",
		   "display HTML files at this platform"],
	    notify(Parent, Str);
	true ->
	    {error, Reason} = Cmd,
	    Str = file:format_error(Reason),
	    notify(Parent, [File,Str])
    end.

%% file_type(File) -> local | remote | {error,Reason}
%%   File = string()
%%   Reason - see file(3)
%% Returns local if File is an existing, readable file
%% Returns remote if File is a remote URL (ie begins with 'http:')
file_type(File) ->
    case File of
	"http://"++_URL ->
	    remote;
	_ ->
	    %% HTML files can have a tag (<name>.html#tag), this must be
	    %% removed when checking if the file exists
	    File2 = case filename:extension(File) of
			".html#"++_Index ->
			    filename:rootname(File)++".html";
			_ ->
			    File
		    end,

            case file:read_file_info(File2) of
	        {ok, FileInfo} when FileInfo#file_info.type==regular,
				    FileInfo#file_info.access/=none ->
		    local;
		{ok, FileInfo} when FileInfo#file_info.type/=regular ->
		    {error,einval};
		{ok, FileInfo} when FileInfo#file_info.access==none ->
		    {error,eacces};
		Error ->
		    Error
	    end
    end.


%%----------------------------------------------------------------------
%% file_dialog(Options) -> tbd
%%----------------------------------------------------------------------
file_dialog(Options) ->
    tool_file_dialog:start(Options).


%%----------------------------------------------------------------------
%% notify(Parent, Strings) -> ok
%% confirm(Parent, Strings) -> ok | cancel
%% confirm_yesno(Parent, Strings) -> yes | no | cancel
%% request(Parent, Strings) -> {ok,string()} | cancel
%%   Parent = gsobj()  (GS root object or parent window)
%%   Strings = string() | [string()]
%% Opens a window with the specified message (Strings) and locks the GUI
%% until the user confirms the message.
%% If the Parent argument is the parent window, the help window will be
%% centered above it, otherwise it can end up anywhere on the screen.
%% A 'notify' window contains an 'Ok' button.
%% A 'confirm' window contains an 'Ok' and a 'Cancel' button.
%% A 'confirm_yesno' window contains a 'Yes', a 'No', and a 'Cancel'
%% button.
%% A 'request' window contains an entry, an 'Ok' and a 'Cancel' button.
%%----------------------------------------------------------------------
-define(Wlbl, 130).
-define(Hlbl, 30).
-define(Hent, 30).
-define(Wbtn, 50).
-define(Hbtn, 30).
-define(PAD,  10).

notify(Parent, Strings) ->
     help_win(notify, Parent, Strings).
confirm(Parent, Strings) ->
    help_win(confirm, Parent, Strings).
confirm_yesno(Parent, Strings) ->
    help_win(confirm_yesno, Parent, Strings).
request(Parent, Strings) ->
    help_win(request, Parent, Strings).

help_win(Type, Parent, Strings) ->
    GenOpts = [{keypress,true}],
    GenOpts2 = [{font,{screen,12}} | GenOpts],
    Buttons = buttons(Type),
    Nbtn = length(Buttons),

    %% Create the window and its contents
    Win = gs:create(window, Parent, [{title,title(Type)} | GenOpts]),
    Top = gs:create(frame, Win, GenOpts),
    Lbl = gs:create(label, Top, [{align,c}, {justify,center}|GenOpts2]),
    Mid = if
	      Type==request -> gs:create(frame, Win, GenOpts);
	      true -> ignore
	  end,
    Ent = if
	      Type==request ->
		  Events = [{setfocus,true},
			    {focus,true},{enter,true},{leave,true}],
		  gs:create(entry, Mid, GenOpts2++Events);
	      true -> ignore
	  end,
    Bot = gs:create(frame, Win, GenOpts),

    %% Find out minimum size required for label, entry and buttons
    Font = gs:read(Parent, {choose_font, {screen,12}}),
    Text = insert_newlines(Strings),
    {Wlbl0,Hlbl0} = gs:read(Lbl, {font_wh,{Font,Text}}),
    {_Went0,Hent0} = gs:read(Lbl, {font_wh,{Font,"Entry"}}),
    {Wbtn0,Hbtn0} = gs:read(Lbl, {font_wh,{Font,"Cancel"}}),
    
    %% Compute size of the objects and adjust the graphics accordingly
    Wbtn = erlang:max(Wbtn0+10, ?Wbtn),
    Hbtn = erlang:max(Hbtn0+10, ?Hbtn),
    Hent = erlang:max(Hent0+10, ?Hent),
    Wlbl = erlang:max(Wlbl0, erlang:max(Nbtn*Wbtn+(Nbtn-1)*?PAD, ?Wlbl)),
    Hlbl = erlang:max(Hlbl0, ?Hlbl),

    Wwin = ?PAD+Wlbl+?PAD,

    Htop = ?PAD+Hlbl,
    Hmid = if Type==request -> ?PAD+Hent; true -> 0 end,
    Hbot = ?PAD+Hbtn+?PAD,
    Hwin = Htop+Hmid+Hbot,

    case catch get_coords(Parent, Wwin, Hwin) of
	{Xw, Yw} when is_integer(Xw), is_integer(Yw) ->
	    gs:config(Win, [{x,Xw}, {y,Yw}]);
	_ ->
	    ignore
    end,
	    
    gs:config(Win, [                       {width,Wwin},{height,Hwin}]),

    gs:config(Top, [{x,0},   {y,0},        {width,Wwin},{height,Htop}]),
    gs:config(Lbl, [{x,?PAD},{y,?PAD},     {width,Wlbl},{height,Hlbl}]),

    gs:config(Mid, [{x,0},   {y,Htop},     {width,Wwin},{height,Hmid}]),
    gs:config(Ent, [{x,?PAD},{y,?PAD},     {width,Wlbl},{height,Hent}]),

    gs:config(Bot, [{x,0},   {y,Htop+Hmid},{width,Wwin},{height,Hbot}]),

    %% Insert the label text
    gs:config(Lbl, {label,{text,Text}}),

    %% Add the buttons
    Xbtns = xbuttons(Buttons, Wbtn, Wwin, Wlbl),
    BtnObjs =
	lists:map(fun({Btext,BX}) ->
			  gs:create(button, Bot, [{x,BX-1}, {y,?PAD-1},
						  {width,Wbtn+2},
						  {height,Hbtn+2},
						  {label,{text,Btext}},
						  {data,data(Btext)}
						  | GenOpts2])
		  end,
		  Xbtns),
    Highlighted = highlight(undef, 1, BtnObjs),

    gs:config(Win, [{map,true}]),

    State = if
		Type==request ->
		    #state{in_focus=1, is_cursor=false};
		true ->
		    #state{}
	    end,
    event_loop(State#state{type=Type, win=Win, entry=Ent,
			   buttons=BtnObjs, highlighted=Highlighted}).

title(notify) ->        "Notification";
title(confirm) ->       "Confirmation";
title(confirm_yesno) -> "Confirmation";
title(request) ->       "Request".

buttons(notify) ->        ["Ok"];
buttons(confirm) ->       ["Ok", "Cancel"];
buttons(confirm_yesno) -> ["Yes", "No", "Cancel"];
buttons(request) ->       ["Ok", "Cancel"].

data("Ok") ->     {helpwin,ok};
data("Yes") ->    {helpwin,yes};
data("No") ->     {helpwin,no};
data("Cancel") -> {helpwin,cancel}.

get_coords(Parent, W, H) ->
    case gs:read(Parent, x) of
	X when is_integer(X) ->
	    case gs:read(Parent, y) of
		Y when is_integer(Y) ->
		    case gs:read(Parent, width) of
			W0 when is_integer(W0) ->
			    case gs:read(Parent, height) of
				H0 when is_integer(H0) ->
				    {round((X+W0/2)-W/2),
				     round((Y+H0/2)-H/2)};
				_ -> error
			    end;
			_ -> error
		    end;
		_ -> error
	    end;
	_ -> error
    end.

xbuttons([B], Wbtn, Wwin, _Wlbl) ->
    [{B, round(Wwin/2-Wbtn/2)}];
xbuttons([B1,B2], Wbtn, Wwin, Wlbl) ->
    Margin = (Wwin-Wlbl)/2,
    [{B1,round(Margin)}, {B2,round(Wwin-Margin-Wbtn)}];
xbuttons([B1,B2,B3], Wbtn, Wwin, Wlbl) ->
    Margin = (Wwin-Wlbl)/2,
    [{B1,round(Margin)},
     {B2,round(Wwin/2-Wbtn/2)},
     {B3,round(Wwin-Margin-Wbtn)}].

highlight(Prev, New, BtnObjs) when New>0, New=<length(BtnObjs) ->
    if
	Prev==undef -> ignore;
	true ->
	    gs:config(lists:nth(Prev, BtnObjs), [{highlightbw,0}])
    end,
    gs:config(lists:nth(New, BtnObjs), [{highlightbw,1},
					{highlightbg,black}]),
    New;
highlight(Prev, _New, _BtnObjs) -> % New is outside allowed range
    Prev.

event_loop(State) ->
    receive
	GsEvent when element(1, GsEvent)==gs ->
	    case handle_event(GsEvent, State) of
		{continue, NewState} ->
		    event_loop(NewState);

		{return, Result} ->
		    gs:destroy(State#state.win),
		    Result
	    end
    end.

handle_event({gs,_,click,{helpwin,Result},_}, State) ->
    if
	State#state.type/=request; Result==cancel ->
	    {return, Result};

	State#state.type==request, Result==ok ->
	    case gs:read(State#state.entry, text) of
		"" ->
		    {continue, State};
		Info ->
		    {return, {ok, Info}}
	    end
    end;

%% When the entry (Type==request) is in focus and the mouse pointer is
%% over it, don't let 'Left'|'Right' keypresses affect which button is
%% selected
handle_event({gs,Ent,enter,_,_}, #state{entry=Ent}=State) ->
    {continue, State#state{is_cursor=true}};
handle_event({gs,Ent,leave,_,_}, #state{entry=Ent}=State) ->
    {continue, State#state{is_cursor=false}};
handle_event({gs,Ent,focus,_,[Int|_]}, #state{entry=Ent}=State) ->
    {continue, State#state{in_focus=Int}};

handle_event({gs,Win,keypress,_,['Right'|_]}, #state{win=Win}=State) ->
    if
	State#state.type==request,
	State#state.in_focus==1, State#state.is_cursor==true ->
	    {continue, State};
	true ->
	    Prev = State#state.highlighted,
	    New = highlight(Prev, Prev+1, State#state.buttons),
	    {continue, State#state{highlighted=New}}
    end;
handle_event({gs,Win,keypress,_,['Left'|_]}, #state{win=Win}=State) ->
    if
	State#state.type==request,
	State#state.in_focus==1, State#state.is_cursor==true ->
	    {continue, State};
	true ->
	    Prev = State#state.highlighted,
	    New = highlight(Prev, Prev-1, State#state.buttons),
	    {continue, State#state{highlighted=New}}
    end;

handle_event({gs,Ent,keypress,_,['Tab'|_]}, #state{entry=Ent}=State) ->
    gs:config(hd(State#state.buttons), {setfocus,true}),
    gs:config(Ent, {select,clear}),
    {continue, State#state{in_focus=0}};

handle_event({gs,Win,keypress,_,['Return'|_]}, #state{win=Win}=State) ->
    Selected = lists:nth(State#state.highlighted, State#state.buttons),
    Data = gs:read(Selected, data),
    handle_event({gs,Win,click,Data,undef}, State);

handle_event({gs,Win,destroy,_,_}, #state{win=Win}=State) ->
    if
	State#state.type==notify -> {return, ok};
	true -> {return, cancel}
    end;

%% Flush any other GS events
handle_event({gs,_Obj,_Event,_Data,_Arg}, State) ->
    {continue, State}.

%% insert_newlines(Strings) => string()
%%   Strings - string() | [string()]
%% If Strings is a list of strings, return a string where all these
%% strings are concatenated with newlines in between,otherwise return
%% Strings.
insert_newlines([String|Rest]) when is_list(String),Rest/=[]->
    String ++ "\n" ++ insert_newlines(Rest);
insert_newlines([Last]) ->
    [Last];
insert_newlines(Other) ->
    Other.
