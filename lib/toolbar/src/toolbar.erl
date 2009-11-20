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
-module(toolbar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Erlang Toolbar
%
%%% Description %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Main module
%
%%% Includes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-include("toolbar.hrl").
%
%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-export([start/0,version/0]).
-export([update/0,quit/0]).
-export([create_tool_file/0,add_gs_contribs/0]).

%
-define (STARTUP_TIMEOUT, 20000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Exported functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% start() => ok | already_started
%----------------------------------------
start() ->
    case whereis(toolbar) of
	undefined ->
	    Self = self(),
	    PidInit = spawn(fun() -> init(Self)  end),
	    init_ok (PidInit);

	_Pid ->
	    already_started
    end.



%%% init_ok  /1
%%%
%%% init_ok returns the pid from this process given from
%%% init/1 after its initialization, or else it timeouts.
%%%

init_ok (PidInit) ->
    %% Wait for a initialization completion message from
    %% the spawned process before returning its Pid.
    %% 
 
    receive
        {initialization_complete, PidInit} ->
            PidInit
 
    %% (Conditional) Failure to start within the time limit will
    %% result in termination
 
    after
        ?STARTUP_TIMEOUT ->
            exit(PidInit, kill),
            exit({startup_timeout, ?MODULE})
    end.
 


%----------------------------------------
% version() -> string()
% Returns the version number.  
%----------------------------------------
version() ->
    "1.1".

%----------------------------------------
% update() => ok | {error,not_started}
% Make a search for new tools (*.tool files) in the current path.
%----------------------------------------
update() ->
    call(update_toolbar).

%----------------------------------------
% quit() => ok | {error,not_started}
% Quit the Toolbar.
%----------------------------------------
quit() ->
    call(quit).

%----------------------------------------
% create_tool_file() => ok | {error,not_started}
% Start the GUI for creating .tool files.
%----------------------------------------
create_tool_file() ->
    call(create_tool_file).

%----------------------------------------
% add_gs_contribs() => ok | {error,not_started}
% Add GS contributions.
%----------------------------------------
add_gs_contribs() ->
    call(add_gs_contribs).


%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%=============================================================================
% Main loop
%=============================================================================

%----------------------------------------
% init()
%----------------------------------------
init(PidCaller) ->
    register (toolbar, self ()),

    %% Start GS
    S = gs:start([{kernel,true}]),
    
    %% Draw main window
    Window = toolbar_graphics:draw_window(S),

    %% Add system defined Tool icons to main window
    toolbar_graphics:cursor(Window,busy),
    NewWindow = add_tools(Window,code:get_path()),
    toolbar_graphics:cursor(Window,arrow),
    
    %% Listen to configure events from the window
    toolbar_graphics:listen_configure(NewWindow),
    
    %% Notify caller that the process appears
    %% to have been started.
    PidCaller ! {initialization_complete, self()},
     
    loop(S,NewWindow,null,undefined).

%----------------------------------------
% loop(S,Window,LoopData,TimerRef)
%   S - pid() GS
%   Window - tbwindow record (see toolbar_graphics.erl)
%   LoopData - term()
%   TimerRef - undefined | timer_ref()
%----------------------------------------
loop(S,Window,LoopData,TimerRef) ->
    receive
	%% test events
	{ping, Pid} ->
            Pid ! {toolbar, alive},
            loop (S, Window, LoopData, TimerRef);
 
        {stop, Pid} ->
            Pid ! {toolbar, stopped},
	    finished;
 
  	%% ----- GS events ----- %%

	{gs,_Object,Event,Data,Args} ->
	    case toolbar_graphics:event(LoopData,Event,Data,Args) of
		
		noevent ->
		    loop(S,Window,LoopData,TimerRef);

		%% Display short information message
		{display,Msg} ->
		    
		    {ok,Ref} = timer:apply_after(500,toolbar_graphics,
						 display_show,[Window,Msg]),
		    loop(S,Window,LoopData,Ref);

		%% Clear display area
		display_clear ->
		    timer:cancel(TimerRef),
		    toolbar_graphics:display_clear(Window),
		    loop(S,Window,LoopData,undefined);
		
		%% New LoopData
		{newData,NewLoopData} ->
		    loop(S,Window,NewLoopData,TimerRef);

		%% Icon button clicked, start corresponding tool/uc
		{start,Start} ->
		    WinObj = toolbar_graphics:get_window(Window),
		    start_tool(Start,WinObj),
		    loop(S,Window,LoopData,TimerRef);
		
		%% Update Toolbar
		update_toolbar ->
		    toolbar_graphics:cursor(Window,busy),
		    NewWindow = add_tools(Window,code:get_path()),
		    toolbar_graphics:cursor(Window,arrow),
		    loop(S,NewWindow,LoopData,TimerRef);

		%% Start Tool Configuration tool
		create_tool_file ->
		    toolbar_toolconfig:start(),
		    loop(S,Window,LoopData,TimerRef);
		
		%% Add GS contributions
		add_gs_contribs ->
		    toolbar_graphics:cursor(Window,busy),
		    GsDir = toolbar_lib:gs_contribs_dir(),
		    code:add_path(GsDir),
		    NewWindow = add_tools(Window,[GsDir]),
		    toolbar_graphics:cursor(Window,arrow),
		    loop(S,NewWindow,LoopData,TimerRef);
		
		%% Help
		{help,Html} ->
		    toolbar_graphics:cursor(Window,busy),
		    WinObj = toolbar_graphics:get_window(Window),
		    tool_utils:open_help(WinObj, Html),
		    toolbar_graphics:cursor(Window,arrow),
		    loop(S,Window,LoopData,TimerRef);
		
		%% About help
		about_help ->
		    WinObj = toolbar_graphics:get_window(Window),
		    Text = ["Help text is on HTML format",
			    "Requires Netscape to be up and running"],
		    tool_utils:notify(WinObj, Text),
		    loop(S,Window,LoopData,TimerRef);

		%% Window has been resized, redraw it
		{redraw,Size} ->
		    NewWindow = toolbar_graphics:redraw_window(Window,Size),
		    loop(S,NewWindow,LoopData,TimerRef);

		%% Quit
		quit ->
		    finished
	    end;

	%% ----- Events from user ----- %%
	
	%% Update Toolbar
	update_toolbar ->
	    toolbar_graphics:cursor(Window,busy),
	    NewWindow = add_tools(Window,code:get_path()),
	    toolbar_graphics:cursor(Window,arrow),
	    loop(S,NewWindow,LoopData,TimerRef);
	
	%% Quit
	quit ->
	    finished;

	%% Start Tool Configuration tool
	create_tool_file ->
	    toolbar_toolconfig:start(),
	    loop(S,Window,LoopData,TimerRef);
		
	%% Add GS contributions
	add_gs_contribs ->
	    toolbar_graphics:cursor(Window,busy),
	    GsDir = toolbar_lib:gs_contribs_dir(),
	    code:add_path(GsDir),
	    NewWindow = add_tools(Window,[GsDir]),
	    toolbar_graphics:cursor(Window,arrow),
	    loop(S,NewWindow,LoopData,TimerRef);
	
	Other ->
	    io:format("toolbar: unexp msg ~p~n",[Other]),
	    loop(S,Window,LoopData,TimerRef)
    end.

%----------------------------------------
% call(Msg) => ok | {error,not_started}
%   Msg - term()
% Send message to toolbar if it is started, otherwise return an error
%----------------------------------------
call(Msg) ->
    case whereis(toolbar) of
	undefined ->
	    {error,not_started};
	_ ->
	    toolbar ! Msg,
	    ok
    end.


%=============================================================================
% Addition of new tools
%=============================================================================
%----------------------------------------
% add_tools(Window,Dirs) => NewWindow
%   Window, NewWindow - tbwindow record (see toolbar_graphics.erl)
%   Dirs - [string()] Directory names
% Calls add_tools2/2 recursively for a number of directories
%----------------------------------------
add_tools(Window,[Dir|Rest]) when is_list(Dir) ->

    %% Add all tools in the directory Dir
    NewWindow = add_tools2(Window,tool_files(Dir)),

    case filename:basename(Dir) of
	%% Dir is an 'ebin' directory, check in '../priv' as well
	"ebin" ->
	    NewerWindow =
		add_tools2(NewWindow,
			   tool_files(filename:join(filename:dirname(Dir),
						    "priv"))),
	    add_tools(NewerWindow,Rest);
	_ ->
	    add_tools(NewWindow,Rest)
    end;
add_tools(Window,[]) ->
    Window.

%----------------------------------------
% add_tools2(Window,ToolFiles) => NewWindow
%   Window, NewWindow - tbwindow record (see toolbar_graphics.erl)
%   ToolFiles - [string()] *.tool file names
% Calls add_tool/2 recursively for a number of .tool files in a directory
%----------------------------------------
add_tools2(Window,[ToolFile|Rest]) ->
    case add_tool(Window,ToolFile) of
	{ok,NewWindow} ->
	    add_tools2(NewWindow,Rest);
	{error,_Reason} ->
	    add_tools2(Window,Rest)
    end;
add_tools2(Window,[]) ->
    Window.

%----------------------------------------
% add_tool(Window,ToolFile) => {ok,NewWindow} | {error,Reason}
%   Window, NewWindow - tbwindow record (see toolbar_graphics.erl)
%   ToolFile - string() A .tool file
%   Reason - noname | nostart | version | format | read | open
% Reads tool information from a .tool file and adds it to the toolbar
% Returns the new window information
%----------------------------------------
add_tool(Window,ToolFile) ->
    case tool_info(ToolFile) of
	{ok,ToolInfo} ->
	    case toolbar_graphics:already_added(Window,ToolInfo) of
		true ->
		    {ok,Window};
		false ->
		    NewWindow = toolbar_graphics:add_icon(Window,ToolInfo),
		    {ok,NewWindow}
	    end;
	{error,Reason} ->
	    %% Log
	    {error,Reason}
    end.


%=============================================================================
% Functions for getting *.tool configuration files
%=============================================================================

%----------------------------------------
% tool_files(Dir) => ToolFiles
%   Dir - string() Directory name
%   ToolFiles - [string()]
% Return the list of all files in Dir ending with .tool (appended to Dir)
%----------------------------------------
tool_files(Dir) ->
    case file:list_dir(Dir) of
	{ok,Files} ->
	    filter_tool_files(Dir,Files);
	{error,_Reason} ->
	    []
    end.

%----------------------------------------
% filter_tool_files(Dir,Files) => ToolFiles
%   Dir - string() Directory name
%   Files, ToolFiles - [string()] File names
% Filters out the files in Files ending with .tool and append them to Dir
%----------------------------------------
filter_tool_files(_Dir,[]) ->
    [];
filter_tool_files(Dir,[File|Rest]) ->
    case filename:extension(File) of
	".tool" ->
	    [filename:join(Dir,File)|filter_tool_files(Dir,Rest)];
	_ ->
	    filter_tool_files(Dir,Rest)
    end.


%=============================================================================
% Functions for retrieving tool information from *.tool files
%=============================================================================

%----------------------------------------
% tool_info(ToolFile) => {ok,ToolInfo} | {error,Reason}
%   ToolFile - string() .tool file
%   ToolInfo - toolinfo record
%   Reason - nofile | format | noname | nostart
% Retreives tool information from ToolFile
%----------------------------------------
tool_info(ToolFile) ->
    case file:consult(ToolFile) of
	{error,open} ->
	    {error,nofile};
	{error,read} ->
	    {error,format};
	{ok,[{version,Vsn},InfoTuple]} when is_tuple(InfoTuple)->
	    case toolbar_lib:tool_info_syntax(Vsn,InfoTuple) of
		
		%% Syntax check ok, start additional checks
		{ok,InfoList} ->

		    tool_info2(filename:dirname(ToolFile),
			       InfoList,#toolinfo{});
		
		%% Syntax error
		Error ->
		    Error
	    end;
	{ok,[{version,Vsn},ToolInfo]} when is_list(ToolInfo)->
	    case toolbar_lib:tool_info_syntax(Vsn,ToolInfo) of
		
		%% Syntax check ok, start additional checks
		{ok,InfoList} ->
		    tool_info2(filename:dirname(ToolFile),
			       InfoList,#toolinfo{});
		
		%% Syntax error
		Error ->
		    Error
	    end;
	{ok,_Other} ->
	    {error,format}
    end.

%----------------------------------------
% tool_info2(Dir,Info,ToolInfo) => {ok,ToolInfo}
%   Dir - string() Directory where this .tool file is situated
%   Info - [{Key,Val}] List of tuples in the .tool file
%   ToolInfo - toolinfo record being filled in
% Used by tool_info2/1
%----------------------------------------
%%% Tool name
tool_info2(Dir,[{tool,Name}|Rest],TI) ->
    tool_info2(Dir,Rest,TI#toolinfo{tool=Name});

%%% Start function
tool_info2(Dir,[{start,{M,F,A}}|Rest],TI) ->
    tool_info2(Dir,Rest,TI#toolinfo{start={M,F,A}});

%%% Icon file
%%% It must exist since the icon is drawn immediately after this .tool
%%% file has been successfully read
%%% It must also end with a .gif or .xbm suffix
%%% Otherwise the icon is ignored!
%%% Uses absolute path: If a relative path is given, it is assumed to be
%%% relative to the directory of the .tool file
tool_info2(Dir,[{icon,Icon}|Rest],TI) ->

    %% Check that the image file ends with .xbm or .gif
    case image_suffix(Icon) of
	true ->

	    %% Add absolute path (if necessary)
	    File = absolute_path(Dir,Icon),
		
	    case toolbar_lib:legal_file(File) of
		ok ->
		    tool_info2(Dir,Rest,TI#toolinfo{icon=File});
		_Error ->
		    %% LOG File does not exist or cannot be read
		    tool_info2(Dir,Rest,TI)
	    end;
		
	false ->
	    %% LOG Illegal icon file name
	    tool_info2(Dir,Rest,TI)
    end;

%%% Message string
tool_info2(Dir,[{message,Msg}|Rest],TI) ->
    tool_info2(Dir,Rest,TI#toolinfo{message=Msg});

%%% Html file is found
%%% Check if file exists at "view-time", not now!
%%% Uses absolute path: If a relative path is given, it is assumed to be
%%% relative to the directory of the .tool file
tool_info2(Dir,[{html,Html}|Rest],TI) ->
    
    %% Check if the HTML file is a remote URL or a local file
    case Html of

	%% http://... Remote URL, save as is
	[$h,$t,$t,$p,$:,$/,$/|_] ->
	    tool_info2(Dir,Rest,TI#toolinfo{html=Html});

	%% file:... Local file, save file with absolute path
	[$f,$i,$l,$e,$:|File] ->
	    tool_info2(Dir,Rest,TI#toolinfo{html=absolute_path(Dir,File)});

	%% Everything else is assumed to be a file name
	%% Save file with absolute path
	_ ->
	    tool_info2(Dir,Rest,TI#toolinfo{html=absolute_path(Dir,Html)})
    end;

%%% Info has been traversed
tool_info2(_Dir,[],ToolInfo) ->
    {ok,ToolInfo}.
		       
%----------------------------------------
% image_suffix(File) => true | false
%   File - string() File name
% Returns true if File end with an image suffix: gif or xbm
%----------------------------------------
image_suffix(File) ->
    case filename:extension(File) of
	".gif" ->
	    true;
	".xbm" ->
	    true;
	_ ->
	    false
    end.

%----------------------------------------
% absolute_path(Dir,File) => string()
%   Dir, File - string()
% Given a directory and a file name, return the appended result if the file
% name does not already contain an absolute path.
% Dir is supposed to be an absolute path, if it is '.', it is replaced
% with the current working directory.
%----------------------------------------
absolute_path(".",File) ->
    {ok,Cwd} = file:get_cwd(),
    absolute_path(Cwd,File);
absolute_path(Dir,File) ->
    filename:join(Dir,File).


%=============================================================================
% Start of a tool
%=============================================================================

%----------------------------------------
% start_tool({Module,Function,Arguments}, GSobj)
%   Module - atom() Module name
%   Function - atom() Function name
%   Argument - [term()] Function arguments
%   GSobj - gs_obj()
% Applies the given function in order to start a tool.
%----------------------------------------
start_tool({M,F,A}, GSobj) ->
    spawn(fun() -> start_tool(M, F, A, GSobj) end).

start_tool(M,F,A,GSobj) ->
    case catch apply(M,F,A) of
	{'EXIT',Reason} ->
	    String1 = io_lib:format("Failed to call apply(~p,~p,~p)",
				    [M,F,A]),
	    String2 = io_lib:format("Reason: ~p",[Reason]),
	    tool_utils:notify(GSobj,[String1,String2]),
	    false;
	_ ->
	    true
    end.
