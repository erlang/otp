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
-module(toolbar_toolconfig).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Erlang Toolbar
%
%%% Description %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Tool configuration tool, edit and creates .tool files
% This tool works separately from the toolbar.
%
%%% External data types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% toolinfo() -- Tool configuration information
-include("toolbar.hrl").
%
%%% Internal data types %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% tfwindow() -- Toolfile configuration window
-record(tfwindow,
	{window,
	 fileentry,
	 toolentry,moduleentry,functionentry,
	 iconentry,messageentry,htmlentry,
	 label}).
%
%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-export([start/0]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/0]). % spawn


%%% Exported functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% start() => pid()
%----------------------------------------
start() ->
    spawn(toolbar_toolconfig,init,[]).


%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%=============================================================================
% Main loop
%=============================================================================

%----------------------------------------
% init()
%----------------------------------------
init() ->

    %% Start GS (or get the pid if it is already running)
    S = gs:start(),

    %% Draw the window
    Window = draw_window(S),

    loop(S,Window).

%----------------------------------------
% loop(S,Window)
%   S - pid() GS
%   Window - tfwindow()
%----------------------------------------
loop(S,Window) ->
    receive

	%% 'Return' pressed in the 'File' entry
	{gs,_Obj,keypress,file,['Return'|_]} ->

	    %% Check if a file name is specified
	    case string:strip(gs:read(Window#tfwindow.fileentry,text)) of
		
		%% No file name specified, move focus to next entry
		"" ->
		    move_focus(Window,file);

		%% A name is specified
		String ->
		    
		    %% Add a .tool suffix to the file name if necessary
		    FileName = tool_file(String),
		    
		    %% Write the complete file name to the file entry
		    gs:config(Window#tfwindow.fileentry,{text,FileName}),
		    
		    %% Try to open the file
		    case file:consult(FileName) of
			
			%% File exists and seems ok
			{ok,[{version,Vsn},T]} ->
			    
			    %% Check the syntax of the file contents
			    %% (All mandatory information specified,
			    %%  correct types, etc)
			    case toolbar_lib:tool_info_syntax(Vsn,T) of
				
				%% Ok -- Show the file contents in the window
				%% and move focus to the next entry
				{ok,Info} ->
				    display(Window,"File: "++FileName++
					           " opened"),
				    clear_info(Window),
				    show_info(Window,Info),
				    move_focus(Window,file);
				
				%% Erronous version number -- Notify user
				{error,version} ->
				    Win = Window#tfwindow.window,
				    tool_utils:notify(Win,[FileName,
				       "File has wrong version number"]);

				%% Other error -- Notify user
				_Error ->
				    Win = Window#tfwindow.window,
				    tool_utils:notify(Win,[FileName,
				       "File is on erronous format"])
			    end;
				
			%% The file can not be read, show default values
			%% according to the file name in the window and
			%% move focus to the next entry
			_ ->
			    display(Window,"File: "++FileName ++
				    " could not be read, new file"),
			    Tool = filename:basename(FileName,".tool"),
			    clear_info(Window),
			    show_info(Window,[{tool,Tool},
					      {start,{list_to_atom(Tool),
						      start,[]}},
					      {icon,Tool++".gif"},
					      {html,Tool++".html"}]),
			    move_focus(Window,file)
		    end
	    end,
	    loop(S,Window);

	%% 'Return' pressed in another entry, move focus to next entry
	{gs,_Obj,keypress,Focus,['Return'|_]} ->
	    move_focus(Window,Focus),
	    loop(S,Window);
	
	%% Any oher keypress, clear the display
	{gs,_Obj,keypress,_Data,_Args} ->
	    display_clear(Window),
	    loop(S,Window);

	%% 'Clear' button pressed, clear the window
	{gs,_Obj,click,_Data,["Clear"|_]} ->
	    clear_info(Window),
	    loop(S,Window);

	%% 'Save' button pressed, save the given information to file
	{gs,_Obj,click,_Data,["Save"|_]} ->

	    %% Check if a file name is specified
	    case string:strip(gs:read(Window#tfwindow.fileentry,text)) of
		
		%% No file name specified, notify user
		"" ->
		    Win = Window#tfwindow.window,
		    tool_utils:notify(Win,
				      "A file name must be specified");

		%% A name is specified
		String ->
		    
		    %% Add a .tool suffix to the file name if necessary
		    FileName = tool_file(String),
		    
		    %% Write the complete file name to the file entry
		    gs:config(Window#tfwindow.fileentry,{text,FileName}),
		    
		    %% Check the other information given
		    case check_info(Window) of
		
			%% If given info is correct, try to save
			%% it to the file
			{ok,ToolInfo} ->
			    Win = Window#tfwindow.window,
			    case save_info(Win,FileName,ToolInfo) of

				%% Ok, display confirmation
				ok ->
				    display(Window,
					    "Tool information saved to "++
					    FileName);

				%% Cancel, do nothing
				cancel ->
				    ignore;
				
				%% Error, display error message
				{error,Reason} ->
				    display(Window,
					    toolbar_lib:error_string(Reason)++
					    FileName)
			    end;

			%% Given info incorrect, notify user
			{error,Reason} ->
			    Win = Window#tfwindow.window,
			    Str = toolbar_lib:error_string(Reason),
			    tool_utils:notify(Win,Str)
		    end
	    end,
	    loop(S,Window);

	%% 'Stop' button, close window and exit
	{gs,_Obj,click,_Data,["Stop"|_]} ->
	    gs:destroy(Window#tfwindow.window),
	    finished;

	%% Window closed, exit
	{gs,_Obj,destroy,_Data,_Args} ->
	    finished;

	Other ->
	    io:format("toolbar_toolconfig: unexp msg: ~p~n",[Other]),
	    loop(S,Window)
    end.


%=============================================================================
% Graphics
%=============================================================================

%----------------------------------------
% draw_window(S)
%   S - pid() GS
% Draw the main window.
%----------------------------------------
draw_window(S) ->

    %% -----   Open a new window   ----- 
    Win = gs:create(window,S,[{width,400},{height,390},
			      {title,"Create Tool File"}]),

    %% -----   Top frame containing a 'File name' label and entry   -----
    Top = gs:create(frame,Win,[{x,0},{y,0},{width,400},{height,60},{bw,2},
			       {keypress,true}]),

    %% File name
    gs:create(label,Top,[{x,10},{y,10},{width,80},{height,30},{align,e},
			 {keypress,true},
			 {label,{text,"File name:"}}]),
    File = gs:create(entry,Top,[{x,110},{y,10},{width,280},{height,30},
				{keypress,true},{data,file}]),

    %% -----   Middle frame containing other labels and entries   -----
    Mid = gs:create(frame,Win,[{x,0},{y,60},{width,400},{height,250},{bw,2},
			       {keypress,true}]),

    %% Tool name
    gs:create(label,Mid,[{x,10},{y,10},{width,80},{height,30},{align,e},
			 {keypress,true},
			 {label,{text,"Tool name:"}}]),
    Tool = gs:create(entry,Mid,[{x,110},{y,10},{width,280},{height,30},
				{keypress,true},{data,tool}]),
    
    %% Start function
    gs:create(label,Mid,[{x,10},{y,60},{width,80},{height,30},{align,e},
			 {keypress,true},
			 {label,{text,"Start:"}}]),
    Mod = gs:create(entry,Mid,[{x,110},{y,60},{width,135},{height,30},
			       {keypress,true},{data,module}]),
    Fun = gs:create(entry,Mid,[{x,245},{y,60},{width,135},{height,30},
			       {keypress,true},{data,function}]),
    
    %% Icon file
    gs:create(label,Mid,[{x,10},{y,110},{width,80},{height,30},{align,e},
			 {keypress,true},
			 {label,{text,"Icon file:"}}]),
    Icon = gs:create(entry,Mid,[{x,110},{y,110},{width,280},{height,30},
				{keypress,true},{data,icon}]),
    
    %% Message
    gs:create(label,Mid,[{x,10},{y,160},{width,80},{height,30},{align,e},
			 {keypress,true},
			 {label,{text,"Message:"}}]),
    Msg = gs:create(entry,Mid,[{x,110},{y,160},{width,280},{height,30},
			       {keypress,true},{data,message}]),
    
    %% HTML file
    gs:create(label,Mid,[{x,10},{y,210},{width,80},{height,30},{align,e},
			 {keypress,true},
			 {label,{text,"HTML:"}}]),
    Html = gs:create(entry,Mid,[{x,110},{y,210},{width,280},{height,30},
				{keypress,true},{data,html}]),

    %% -----   Bottom frame containing the buttons   -----
    Bot = gs:create(frame,Win,[{x,0},{y,310},{width,400},{height,50},
			       {bw,2},{keypress,true}]),

    gs:create(button,Bot,[{x,75},{y,10},{width,50},{height,30},
			  {keypress,true},
			  {label,{text,"Clear"}}]),
    gs:create(button,Bot,[{x,175},{y,10},{width,50},{height,30},
			  {keypress,true},
			  {label,{text,"Save"}}]),
    gs:create(button,Bot,[{x,275},{y,10},{width,50},{height,30},
			  {keypress,true},
			  {label,{text,"Stop"}}]),

    %% -----   Label for displaying help messages   -----
    Lbl = gs:create(label,Win,[{x,0},{y,360},{width,400},{height,30},{bw,2},
			       {relief,raised},
			       {keypress,true},
			       {align,c},{label,{text,""}}]),

    gs:config(Win,{map,true}),
    gs:config(File,{setfocus,true}),

    #tfwindow{window=Win,
	      fileentry=File,
	      toolentry=Tool,
	      moduleentry=Mod,
	      functionentry=Fun,
	      iconentry=Icon,
	      messageentry=Msg,
	      htmlentry=Html,
	      label=Lbl}.

%----------------------------------------
% move_focus(Window,Focus)
%   Window - tfwindow()
%   Focus - file | tool | module | function | icon | message | html | none
% Move the input focus to the entry following Focus
%----------------------------------------
move_focus(Window,file) ->
    gs:config(Window#tfwindow.toolentry,{setfocus,true});
move_focus(Window,tool) ->
    gs:config(Window#tfwindow.moduleentry,{setfocus,true});
move_focus(Window,module) ->
    gs:config(Window#tfwindow.functionentry,{setfocus,true});
move_focus(Window,function) ->
    gs:config(Window#tfwindow.iconentry,{setfocus,true});
move_focus(Window,icon) ->
    gs:config(Window#tfwindow.messageentry,{setfocus,true});
move_focus(Window,message) ->
    gs:config(Window#tfwindow.htmlentry,{setfocus,true});
move_focus(Window,html) ->
    gs:config(Window#tfwindow.htmlentry,{setfocus,false});
move_focus(_Window,none) ->
    true.

%----------------------------------------
% display(Window,Text)
%   Window - tfwindow()
%   Text - string()
% Display a help message in the window
%----------------------------------------
display(Window,Text) ->
    gs:config(Window#tfwindow.label,{label,{text,Text}}).
    
%----------------------------------------
% display_clear(Window)
%   Window - tfwindow()
% Clear the help message display
%----------------------------------------
display_clear(Window) ->
    display(Window,"").

%----------------------------------------
% clear_info(Window)
%   Window - tfwindow()
% Clear the entries of Window (except the file entry)
%----------------------------------------
clear_info(Window) ->
    gs:config(Window#tfwindow.toolentry,{text,""}),
    gs:config(Window#tfwindow.moduleentry,{text,""}),
    gs:config(Window#tfwindow.functionentry,{text,""}),
    gs:config(Window#tfwindow.iconentry,{text,""}),
    gs:config(Window#tfwindow.messageentry,{text,""}),
    gs:config(Window#tfwindow.htmlentry,{text,""}).

%----------------------------------------
% show_info(Window,List)
%   Window - tfwindow()
%   List - [{Key,Val}]
%     Key - tool,     Val - string()
%     Key - start,    Val - {atom(),atom(),_}
%     Key - icon,     Val - string()
%     Key - message,  Val - string()
%     Key - html,     Val - string()
% Display the different Val's in the appropriate entries of Window
%----------------------------------------
show_info(_Window,[]) ->
    ok;
show_info(Window,[{tool,Tool}|Rest]) ->
    gs:config(Window#tfwindow.toolentry,{text,Tool}),
    show_info(Window,Rest);
show_info(Window,[{start,{M,F,_}}|Rest]) ->
    gs:config(Window#tfwindow.moduleentry,{text,M}),
    gs:config(Window#tfwindow.functionentry,{text,F}),
    show_info(Window,Rest);
show_info(Window,[{icon,Icon}|Rest]) ->
    gs:config(Window#tfwindow.iconentry,{text,Icon}),
    show_info(Window,Rest);
show_info(Window,[{message,Message}|Rest]) ->
    gs:config(Window#tfwindow.messageentry,{text,Message}),
    show_info(Window,Rest);
show_info(Window,[{html,Html}|Rest]) ->
    gs:config(Window#tfwindow.htmlentry,{text,Html}),
    show_info(Window,Rest).


%=============================================================================
% Retrieve user specified information
%=============================================================================
    
%----------------------------------------
% check_info(Window) => {ok,ToolInfo} | {error,Reason}
%   Window - tfwindow()
%   ToolInfo - toolinfo()
%   Reason - noname | nostart
% Check the information given in the entries and insert it into ToolInfo
% if all mandatory information is given.
%----------------------------------------
check_info(Window) ->

    %% First check mandatory elements: name and start function
    Tool = gs:read(Window#tfwindow.toolentry,text),
    M = gs:read(Window#tfwindow.moduleentry,text),
    F = gs:read(Window#tfwindow.functionentry,text),
    
    if
	Tool/="",M/="",F/="" ->
	    ToolInfo =
		#toolinfo{tool=Tool,
			  start={list_to_atom(M),list_to_atom(F),[]},
			  icon=gs:read(Window#tfwindow.iconentry,text),
			  message=gs:read(Window#tfwindow.messageentry,text),
			  html=gs:read(Window#tfwindow.htmlentry,text)},
	    {ok,ToolInfo};

	Tool=="" ->
	    {error,noname};
		
	true ->
	    {error,nostart}
    end.


%=============================================================================
% Save information to file
%=============================================================================

%----------------------------------------
% save_info(Win,File,ToolInfo) => ok | cancel | {error,waccess}
%   Win - GS object
%   File - string()
%   ToolInfo - toolinfo()
% Saves the information in ToolInfo to File on a predefined format.
%----------------------------------------
save_info(Win,File,ToolInfo) ->

    %% First check if file already exists
    case file:read_file_info(File) of
	{ok,_FileInfo} ->

	    %% Request the user to confirm that the file should
	    %% be overwritten
	    case tool_utils:confirm(Win,[File,
					 "exists, will be overwritten"]) of
		ok ->
		    save_info2(File,ToolInfo);
		cancel ->
		    cancel
	    end;

	{error,_Reason} -> % _Reason = "No such file or directory"
	    save_info2(File,ToolInfo)
    end.

%----------------------------------------
% save_info2(File,ToolInfo) => ok | {error,waccess}
%   File - string() File name
%   ToolInfo - toolinfo record
% Called by save_info/3
%----------------------------------------
save_info2(File,ToolInfo) ->
    case file:open(File, [write]) of
	{ok,Fd} ->
	    io:format(Fd,"{version,\"~s\"}.~n",[toolbar:version()]),
	    io:format(Fd,"{{tool,\"~s\"},~n",[ToolInfo#toolinfo.tool]),
	    io:format(Fd," {start,~w}",[ToolInfo#toolinfo.start]),
	    case ToolInfo#toolinfo.icon of
		"" -> ignore;
		Icon -> io:format(Fd,",~n {icon,\"~s\"}",[Icon])
	    end,
	    case ToolInfo#toolinfo.message of
		"" -> ignore;
		Message -> io:format(Fd,",~n {message,\"~s\"}",[Message])
	    end,
	    case ToolInfo#toolinfo.html of
		"" -> ignore;
		Html -> io:format(Fd,",~n {html,\"~s\"}",[Html])
	    end,
	    io:format(Fd,"}.~n",[]),
	    file:close(Fd),
	    ok;
	_Error ->
	    {error,waccess}
    end.


%=============================================================================
% Auxiliary functions
%=============================================================================

%----------------------------------------
% tool_file(File) => string()
%   File - string()
% Return a file name consisting of File with the suffix .tool added,
% if File does not already have this suffix.
%----------------------------------------
tool_file(File) ->
    case filename:extension(File) of
	".tool" -> File;
	_ -> File ++ ".tool"
    end.
