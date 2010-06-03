%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(appmon).
-behaviour(gen_server).

%%%---------------------------------------------------------------------
%%% Appmon main module.
%%% Creates the main window and receives load and application
%%% information from all connected nodes.
%%%---------------------------------------------------------------------

%% External exports
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([handle_call/3, code_change/3]).        % not used

%% Canvas button data
-record(canvasbutton, {text, ul, ll, rect, x, y, w, h}).

%% Options - all the fields are GS radio buttons
-record(options, {single, many, time, queue, prog, linear}).

%% Main window data
-record(win, {name,                             % atom() Monitored node
	      window,                           % gsobj()
	      wwindow,                          % int() Window width
	      hwindow,                          % int() Window height
	      options,                          % #options{}
	      canvas,                           % gsobj()
	      wcanvas,                          % int() Canvas width
	      hcanvas,                          % int() Canvas height
	      l1, l2,                           % gsobj() Canvas lines
	      leds,                             % [gsobj()] Load meter
	      nodelabel,                        % {gsobj(),gsobj()}
	      appobjs=[],                       % [gsobj()] Buttons etc.
	      nodemenu}).                       % gsobj() Node menu

%% Node data
-record(mnode, {name,                           % atom() Node name
		status,                         % alive | dead
		pid,                            % pid()
		apps,                           % [{Pid,App,Descr}]
		load}).                         % {Old, New}

%% Internal state data
-record(state, {gs,                             % pid()
		wins=[],                        % [#win()] GUIs
		window_mode,                    % single | many
		load_mode1,                     % time | queue
		load_mode2,                     % prog | linear
		lbpid,                          % pid()
		mnodes=[]}).                    % [#mnode{}] 

%%%---------------------------------------------------------------------
%%% External exports
%%%---------------------------------------------------------------------

start() ->
     gen_server:start({local, appmon}, ?MODULE, [], []).

stop() ->
    gen_server:cast(appmon, stop).


%%%---------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),

    %% Subscribe to {nodeup,Node} and {nodedown,Node} messages
    net_kernel:monitor_nodes(true),
    
    LbPid = appmon_lb:start(self ()),

    %% Check which remote nodes have appmon code available (OTP-4887)
    NodesOk = lists:filter(fun(Node) -> check_node(Node) end, nodes()),
    Nodes = [node()|NodesOk],

    %% Start monitoring the existing nodes
    MNodes = mk_mnodes(Nodes, LbPid),

    %% Draw the main window
    GS = gs:start([{kernel,true}]),
    GUI = draw_win(GS, node()),

    %% Update the Nodes menu with all known nodes
    lists:foreach(fun(Node) ->
			  display_addnode(GUI, Node)
		  end,
		  Nodes),

    %% Mark the default options as selected in the Options menu
    display_setopt(GUI, single),
    display_setopt(GUI, time),
    display_setopt(GUI, prog),

    {ok, #state{gs=GS, wins=[GUI],
		window_mode=single, load_mode1=time, load_mode2=prog,
		lbpid=LbPid, mnodes=MNodes}}.

check_node(Node) ->
    case rpc:call(Node, code, which, [appmon]) of
	File when is_list(File) ->
	    true;
	_ -> % non_existing (| cover_compiled)
	    false
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(norequest, _From, State) ->
    {reply, null, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.
    
%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% Load information from a node
handle_info({delivery, _Serv, load, Node, Load}, State) ->
    
    %% Update node information
    MNode = get_mnode(Node, State#state.mnodes),
    MNode1 = MNode#mnode{load=Load},
    MNodes = replace_mnode(Node, MNode1, State#state.mnodes),

    %% If Node is currently displayed, update graphics
    case get_win(Node, State#state.wins) of
	{ok, GUI} ->
	    display_load(GUI, Load);
	false ->
	    ignore
    end,
    
    {noreply, State#state{mnodes=MNodes}};

%% Application information from a node
handle_info({delivery, _Serv, app_ctrl, Node, Apps}, State) ->
    
    %% Update node information
    MNode = get_mnode(Node, State#state.mnodes),
    MNode1 = MNode#mnode{apps=Apps},
    MNodes = replace_mnode(Node, MNode1, State#state.mnodes),

    %% If Node is currently displayed, update graphics
    Wins = case get_win(Node, State#state.wins) of
	       {ok, GUI} ->
		   draw_clear(GUI),
		   GUI1 = draw_apps(GUI, Apps),
		   replace_win(Node, GUI1, State#state.wins);
	       false ->
		   State#state.wins
	   end,
    
    appmon_lb:add_apps (State#state.lbpid, Apps, Node),
    {noreply, State#state{wins=Wins, mnodes=MNodes}};

handle_info({nodeup, Node}, State) ->

    %% First, make sure appmon code is available at remode node,
    %% or the node should be ignored (OTP-3591)
    case check_node(Node) of
	true ->

	    %% If this is a previously unknown node, update window's
	    %% 'Nodes' menu
	    case get_mnode(Node, State#state.mnodes) of
		false ->
		    display_addnode(State#state.wins, Node);
		_OldMnode ->
		    ignore
	    end,

	    %% Update node information (=> state is automatically
	    %% changed to 'alive')
	    MNode = mk_mnode(Node, State#state.lbpid),
	    MNodes = replace_mnode(Node, MNode, State#state.mnodes),

	    %% If Node is currently displayed, update graphics
	    case get_win(Node, State#state.wins) of
		{ok, GUI} ->
		    display_nodeup(GUI, Node);
		false ->
		    ignore
	    end,

	    appmon_lb:update_status(State#state.lbpid, Node, alive),
	    {noreply, State#state{mnodes=MNodes}};

	false ->
	    {noreply, State}
    end;

handle_info({nodedown, Node}, State) ->
    
    %% If this is a previously unknown node, ignore the message.
    %% (The situation occurs when failing to connect to another node).
    %% Otherwise, update the node information.
    case get_mnode(Node, State#state.mnodes) of
	false ->
	    {noreply, State};
	MNode ->
	    MNode1 = MNode#mnode{status=dead},
	    MNodes = replace_mnode(Node, MNode1, State#state.mnodes),

	    %% If Node is currently displayed, update graphics
	    Wins = case get_win(Node, State#state.wins) of
		       {ok, GUI} ->
			   display_nodedown(GUI),
			   GUI1 = draw_clear(GUI),
			   replace_win(Node, GUI1, State#state.wins);
		       false ->
			   State#state.wins
		   end,

	    appmon_lb:remove_node(State#state.lbpid, Node),
	    {noreply, State#state{wins=Wins, mnodes=MNodes}}
    end;

%% Application 'button' events
handle_info({gs, _Obj, buttonpress, Data, _Arg}, State) ->
    {canvasbutton, CBtn, _App} = Data,
    press(CBtn),
    {noreply, State};
handle_info({gs, _Obj, buttonrelease, Data, [_,X,Y|_]}, State) ->
    {canvasbutton, CBtn, {application, App, Node}} = Data,
    release(CBtn),

    %% Check that mouse button was released over the button!
    L = CBtn#canvasbutton.x, R = L + CBtn#canvasbutton.w,
    T = CBtn#canvasbutton.y, B = T + CBtn#canvasbutton.h,
    if
	X>L, X<R, Y>T, Y<B ->
	    MNode = get_mnode(Node, State#state.mnodes),
	    {value, {Pid, _App, _Descr}} =
		lists:keysearch(App, 2, MNode#mnode.apps),
	    appmon_a:start(Node, App, Pid);
	true ->
	    ignore
    end,
    {noreply, State};

handle_info({gs, _Button, click, Data, _Arg}, State) ->
    ThisNode = node(),
    case Data of
	
	%% File menu item
	listbox ->
	    appmon_lb:open_win(State#state.lbpid,
			       parse_nodes(State#state.mnodes)),
	    {noreply, State};
	{close, WinObj} ->
	    {ok, GUI} = get_win2(WinObj, State#state.wins),
	    gs:destroy(WinObj),

	    %% Terminate if this was the only open window
	    case remove_win(GUI#win.name, State#state.wins) of
		[] ->
		    {stop, normal, State};
		Wins ->
		    {noreply, State#state{wins=Wins}}
	    end;
	exit ->
	    {stop, normal, State};

	%% Actions menu item
	{action, Action, WinObj} ->
	    {ok, GUI} = get_win2(WinObj, State#state.wins),
	    Node = GUI#win.name,

	    if
		Node==ThisNode ->
		    case Action of
			ping ->
			    %% Ignore - makes no sense to ping yourself
			    ignore;
			_ -> % reboot | restart | stop
			    apply(init, Action, [])
		    end;

		Node/=ThisNode ->
		    case Action of
			ping ->
			    net_adm:ping(Node);
			_ -> % reboot | restart | stop
			    rpc:cast(Node, init, Action, [])
		    end
	    end,
	    {noreply, State};

	%% Options menu item
	{window_mode, Mode} ->
	    
	    %% Update windows so they all show the same options
	    lists:foreach(fun(GUI) ->
				  display_setopt(GUI, Mode)
			  end,
			  State#state.wins),
	    {noreply, State#state{window_mode=Mode}};
	
	{option, Tag, Option} ->
	    
	    %% Update windows so they all show the same options
	    lists:foreach(fun(GUI) ->
				  display_setopt(GUI, Tag)
			  end,
			  State#state.wins),

	    %% Update all appmon_info processes about which kind of
	    %% load data is desired
	    lists:foreach(fun(MNode) ->
				  appmon_info:load(MNode#mnode.pid,
						   MNode#mnode.name,
						   true,
						   Option)
			  end,
			  State#state.mnodes),

	    if
		Tag==time; Tag==queue ->
		    {noreply, State#state{load_mode1=Tag}};
		Tag==prog; Tag==linear ->
		    {noreply, State#state{load_mode2=Tag}}
	    end;

	%% Nodes menu item
	{node, Node, WinObj} ->

	    %% Check first if this window is already displayed
	    case get_win(Node, State#state.wins) of
		{ok, GUI} ->

		    %% Node is already displayed, raise its window
		    gs:config(GUI#win.window, raise),

		    {noreply, State};

		%% Node is not displayed
		false ->

		    %% Redraw existing window or create a new window
		    %% depending on window mode
		    case State#state.window_mode of

			single ->
			    {ok, GUI} =
				get_win2(WinObj, State#state.wins),
			    	    
			    %% Clear window and correct the node name
			    draw_clear(GUI),
			    GUI1 = draw_nodename(GUI, Node),

			    %% Update window with the correct node name
			    %% and the applications running at the node
			    MNode = get_mnode(Node, State#state.mnodes),
			    GUI2 = case MNode#mnode.status of
				       dead ->
					   display_nodedown(GUI1),
					   GUI1;
				       alive ->
					   display_nodeup(GUI1, Node),
					   draw_apps(GUI1,
						     MNode#mnode.apps)
				   end,
			    Wins = replace_win(GUI#win.name, GUI2,
					       State#state.wins),

			    {noreply, State#state{wins=Wins}};

			many ->
			    GUI = draw_win(State#state.gs, Node),

			    %% Update Nodes menu with all known nodes -
			    %% use MNodes to get them in the right order
			    lists:foreach(fun(MNode) ->
						  Name =
						      MNode#mnode.name,
						  display_addnode(GUI,
								  Name)
					  end,
					  State#state.mnodes),

			    %% Mark selected options in the Options menu
			    display_setopt(GUI, many),
			    display_setopt(GUI, State#state.load_mode1),
			    display_setopt(GUI, State#state.load_mode2),

			    %% Add the applications running at the node
			    MNode = get_mnode(Node, State#state.mnodes),

			    GUI1 = case MNode#mnode.status of
				       dead ->
					   display_nodedown(GUI),
					   GUI;
				       alive ->
					   display_nodeup(GUI, Node),
					   draw_apps(GUI,
						     MNode#mnode.apps)
				   end,
			    Wins = [GUI1|State#state.wins],
			    
			    {noreply, State#state{wins=Wins}}
		    end
	    end;

        %% Help menu = Help button
	help ->
	    HelpFile = filename:join([code:lib_dir(appmon),
				     "doc", "html", "part_frame.html"]),
	    case State#state.wins of
		[Win] ->
		    tool_utils:open_help(Win#win.window, HelpFile);
		_ ->
		    tool_utils:open_help(State#state.gs, HelpFile)
	    end,
	    {noreply, State};
		
	_Other ->
	    {noreply, State}
    end;
handle_info({gs, WinObj, configure, _, [WWindow, HWindow|_]}, State) ->
    {ok, GUI} = get_win2(WinObj, State#state.wins),
    GUI1 = draw_resize(GUI, WWindow, HWindow),
    display_scrollbar(GUI1),
    Wins = replace_win(GUI#win.name, GUI1, State#state.wins),
    {noreply, State#state{wins=Wins}};
handle_info({gs, WinObj, destroy, _, _}, State) ->	% OTP-1179
    {ok, GUI} = get_win2(WinObj, State#state.wins),

    %% Terminate if this was the only open window
    case remove_win(GUI#win.name, State#state.wins) of
	[] ->
	    {stop, normal, State};
	Wins ->
	    {noreply, State#state{wins=Wins}}
    end;

handle_info(stop, State) ->
    {stop, normal, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	shutdown ->
	    %% Appmon is being asked to shut down, eg during reboot
	    {stop, Reason, State};
	_ ->
	    case State#state.gs of
		
		%% GS exited, kill appmon
		{0, Pid} ->
		    {stop, normal, State};
	
		_ ->
		    {noreply, State}
	    end
    end;
handle_info(_Info, State) ->
    {noreply, State}.
	
%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, State) ->
    bcast(State#state.mnodes, {kill}),
    appmon_lb:stop(State#state.lbpid),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%---------------------------------------------------------------------
%%% Internal functions
%%%---------------------------------------------------------------------

%%----------------------------------------------------------------------
%% MNode manipulating functions
%%----------------------------------------------------------------------

%% mk_mnodes(Nodes, LbPid) -> MNodes
%%   Nodes -> [atom()]
%%   LbPid -> pid()
%%   MNodes -> [#mnode{}]
mk_mnodes([Node|Nodes], LbPid) ->
    [mk_mnode(Node, LbPid) | mk_mnodes(Nodes, LbPid)];
mk_mnodes([], _LbPid) ->
    [].

mk_mnode(Node, LbPid) ->

    %% Create an appmon process at the node
    {ok, Pid} = appmon_info:start_link(Node, self(), []),
    
    appmon_lb:add_node(LbPid, Node),
    appmon_info:load(Pid, Node, true, [{timeout,1000}]),
    appmon_info:app_ctrl(Pid, Node, true, []),

    #mnode{name=Node, status=alive, pid=Pid}.

%% get_mnode(Node, MNodes) -> MNode | false
%%   Node -> atom()
%%   MNodes -> [#mnode{}]
%%   MNode -> #mnode{}
get_mnode(Node, MNodes) ->
    case lists:keysearch(Node, #mnode.name, MNodes) of
	{value, MNode} ->
	    MNode;
	false ->
	    false
    end.

%% replace_mnode(Node, MNode, MNodes1) -> Mnodes2
%%   Node -> atom()
%%   MNode -> #mnode{}
%%   MNodes1 -> MNodes2 -> [#mnode{}]
%% Replaces, or adds if previously not included, the mnode with name
%% Node in MNodes1 with MNode.
replace_mnode(Node, MNode, [#mnode{name=Node} | MNodes]) ->
    [MNode | MNodes];
replace_mnode(Node, MNode, [MNode2 | MNodes]) ->
    [MNode2 | replace_mnode(Node, MNode, MNodes)];
replace_mnode(_Node, MNode, []) ->
    [MNode].
	

%%----------------------------------------------------------------------
%% GUI list manipulating functions
%%----------------------------------------------------------------------

%% get_win(Node, Wins) -> Win
%%   Node -> atom()
%%   Wins -> [#win{}]
%%   Win -> #win{}
get_win(Node, Wins) ->
    case lists:keysearch(Node, #win.name, Wins) of
	{value, Win} ->
	    {ok, Win};
	false ->
	    false
    end.

%% get_win2(WinObj, Wins) -> Win
%%   Window -> gsobj()
%%   Wins -> [#win{}]
%%   Win -> #win{}
get_win2(WinObj, Wins) ->
    case lists:keysearch(WinObj, #win.window, Wins) of
	{value, Win} ->
	    {ok, Win};
	false ->
	    false
    end.

%% replace_win(Node, Win, Wins) -> Wins2
%%   Node -> atom()
%%   Win -> #win{}
%%   Wins -> Wins2 -> [#win{}]
replace_win(Node, Win, Wins) ->
    lists:keyreplace(Node, #win.name, Wins, Win).

%% remove_win(Node, Wins) -> Wins2
%%   Node -> atom()
%%   Wins -> Wins2 -> [#win{}]
remove_win(Node, Wins) ->
    lists:keydelete(Node, #win.name, Wins).




%%----------------------------------------------------------------------
%% GUI manipulating functions
%%----------------------------------------------------------------------
-define(PAD, 10).                         % Pad between objects
-define(PAD2, 4*?PAD).                    % Pad betw. node lbl and app

-define(hMENUBAR, 25).                    % Note: Hardwired in Tcl/Tk

-define(xNODELBL, 60).                    % Node label
-define(yNODELBL, 35).
-define(hNODELBL, 20).

-define(xMETER, 5).                       % Meter
-define(yMETER, ?yNODELBL).
-define(wMETER, 20).
-define(hMETER, ?hNODELBL + ?PAD + ?PAD2 + ?hBTN).
-define(LEDCOUNT, 16).

-define(xBTN, ?xNODELBL).                 % Application buttons
-define(yBTN, ?yNODELBL + ?hNODELBL + ?PAD + ?PAD2).
-define(wBTN, 70).                        % min width
-define(hBTN, 20).

-define(wCANVAS, 470 + ?wMETER + 3*?PAD). % Canvas
-define(hCANVAS, ?yNODELBL + ?hNODELBL + ?PAD + ?PAD2 + ?hBTN +	2*?PAD).

-define(wWIN, ?wCANVAS).                  % Window
-define(hWIN, ?hMENUBAR + ?hCANVAS).

%%--Main window---------------------------------------------------------

draw_win(GS, Node) ->

    %% Main window
    NodeStr = atom_to_list(Node),
    Win = gs:create(window, GS, [{title,
				  "APPMON: Overview on " ++ NodeStr},
				 {width, ?wWIN}, {height, ?hWIN},
				 {configure, true}]),
    Canvas = gs:create(canvas, Win, [{x, 0}, {y, ?hMENUBAR},
				     {width, ?wCANVAS},
				     {height, ?hCANVAS}]),
    L1 = gs:create(line, Canvas, [{coords,
				   [{0,?yNODELBL-?PAD},
				    {?wCANVAS,?yNODELBL-?PAD}]}]),
    L2 = gs:create(line, Canvas, [{coords,
				   [{0,?hCANVAS-?PAD},
				    {?wCANVAS,?hCANVAS-?PAD}]}]),
    
    %% Standard buttons
    MenuBar = gs:create(menubar, Win, [{height, ?hMENUBAR}]),

    FileMenuBtn = gs:create(menubutton, MenuBar,
			    [{label, {text,"File"}}]),
    FileMenu = gs:create(menu, FileMenuBtn, []),
    gs:create(menuitem, FileMenu, [{label, {text,"Show List Box..."}},
				   {data, listbox}]),
    gs:create(menuitem, FileMenu, [{label, {text, "Close"}},
				   {data, {close, Win}}]),
    gs:create(menuitem, FileMenu, [{itemtype, separator}]),
    gs:create(menuitem, FileMenu, [{label, {text, "Exit"}},
				   {data, exit}]),

    ActionMenuBtn = gs:create(menubutton, MenuBar,
			      [{label,{text,"Actions"}}]),
    ActionMenu = gs:create(menu, ActionMenuBtn, []),
    gs:create(menuitem, ActionMenu, [{label, {text,"Reboot"}},
				     {data, {action, reboot, Win}}]),
    gs:create(menuitem, ActionMenu, [{label, {text,"Restart"}},
				     {data, {action, restart, Win}}]),
    gs:create(menuitem, ActionMenu, [{label, {text,"Stop"}},
				     {data, {action, stop, Win}}]),
    gs:create(menuitem, ActionMenu, [{label, {text,"Ping"}},
				     {data, {action, ping, Win}}]),

    OptMenuBtn = gs:create(menubutton, MenuBar,
			   [{label, {text,"Options"}}]),
    OptMenu = gs:create(menu, OptMenuBtn, []),
    G0 = now(), % Group identity unique per window!
    SMI = gs:create(menuitem, OptMenu, [{label, {text,"One window"}},
					{itemtype, radio}, {group, G0},
					{data, {window_mode, single}}]),
    MMI = gs:create(menuitem, OptMenu, [{label, {text,"Many windows"}},
					{itemtype, radio}, {group, G0},
					{data, {window_mode, many}}]),
    gs:create(menuitem, OptMenu, [{itemtype, separator}]),
    G1 = now(),
    TMI = gs:create(menuitem, OptMenu, [{label, {text,"Load: time"}},
					{itemtype, radio}, {group, G1},
					{data,
					 {option, time,
					  [{load_method,time}]}}]),
    QMI = gs:create(menuitem, OptMenu, [{label, {text,"Load: queue"}},
					{itemtype, radio}, {group, G1},
					{data,
					 {option, queue,
					  [{load_method,queue}]}}]),
    G2 = now(),
    PMI = gs:create(menuitem, OptMenu,
		    [{label, {text,"Load: progressive"}},
		     {itemtype, radio}, {group, G2},
		     {data, {option, prog, [{load_scale,prog}]}}]),
    LMI = gs:create(menuitem, OptMenu, [{label, {text,"Load: linear"}},
					{itemtype, radio}, {group, G2},
					{data,
					 {option, linear,
					  [{load_scale,linear}]}}]),

    NodeMenuBtn = gs:create(menubutton, MenuBar,
			    [{label, {text,"Nodes"}}]),
    NodeMenu = gs:create(menu, NodeMenuBtn, []),

    HelpMenuBtn = gs:create(menubutton, MenuBar,
			    [{label, {text,"Help"}}, {side, right}]),
    HelpMenu = gs:create(menu, HelpMenuBtn, []),
    gs:create(menuitem, HelpMenu, [{label, {text,"Help"}},
				   {data, help}]),

    %% Meter
    HLed = trunc((?hMETER)/(?LEDCOUNT)),
    Leds = draw_leds(?LEDCOUNT, Canvas, ?yMETER, HLed, []),
    leds_down(Leds, ?LEDCOUNT, 0),
    gs:create(text, Canvas, [{coords,
			      [{?xMETER, ?yMETER+HLed*?LEDCOUNT}]},
			     {anchor, nw},
			     {font, {screen,8}},
			     {text, "Load"}]),
    gs:create(text, Canvas, [{coords, [{?xMETER+?wMETER, ?yMETER}]},
			     {anchor, nw},
			     {font, {screen,8}},
			     {text, "Hi"}]),
    gs:create(text, Canvas, [{coords, [{?xMETER+?wMETER,
					?yMETER+HLed*?LEDCOUNT}]},
			     {anchor, w},
			     {font, {screen,8}},
			     {text, "Lo"}]),

    %% Node label
    WNodeLbl = 8*length(NodeStr)+10,
    NLRect = gs:create(rectangle, Canvas,
		       [{coords, [{?xNODELBL,?yNODELBL},
				  {?xNODELBL+WNodeLbl,
				   ?yNODELBL+?hNODELBL}]},
			{fill, black}]),
    Xc = ?xNODELBL + round(WNodeLbl/2),
    Yc = ?yNODELBL + round(?hNODELBL/2),
    NLText = gs:create(text, Canvas, [{text, NodeStr},
				      {fg, {250,235,215}},
				      {coords, [{Xc,Yc}]},
				      {anchor, c}]),
    NodeLbl = {NLRect, NLText},
    
    gs:config(Win, {map, true}),
    #win{name=Node,
	 window=Win, wwindow=?wWIN, hwindow=?hCANVAS,
	 options=#options{single=SMI, many=MMI,
			  time=TMI, queue=QMI, prog=PMI, linear=LMI},
	 canvas=Canvas, wcanvas=?wCANVAS, hcanvas=?hCANVAS,
	 l1=L1, l2=L2, leds=Leds, nodelabel=NodeLbl, nodemenu=NodeMenu}.

draw_leds(N, Canvas, Y, HLed, Leds) when N>0 ->
    Led = gs:create(rectangle, Canvas,
		    [{coords,
		      [{?xMETER,Y}, {?xMETER+?wMETER,Y+HLed}]}]),
    draw_leds(N-1, Canvas, Y+HLed, HLed, [Led | Leds]);
draw_leds(0, _Canvas, _Y, _HLed, Leds) ->
    Leds.

%%--Draw functions------------------------------------------------------
%% Functions that modify the GUI and its data (win{})

%% Display the node name in the window title
%% (The name in the node label is changed by display_nodeup|nodedown)
%% Used when a changing the node to display
draw_nodename(GUI, Node) ->
    NodeStr = atom_to_list(Node),
    gs:config(GUI#win.window,
	      {title, "APPMON: Overview on " ++ NodeStr}),
    GUI#win{name=Node}.

%% Resize the canvas (when the window has been resized)
draw_resize(GUI, W, H) ->
    Hc = H - ?hMENUBAR,
    gs:config(GUI#win.canvas, [{width, W}, {height, Hc}]),
    Yline1 = ?yNODELBL-?PAD,
    Yline2 = ?hCANVAS-?PAD,
    gs:config(GUI#win.l1, [{coords, [{0,Yline1},{W,Yline1}]}]),
    gs:config(GUI#win.l2, [{coords, [{0,Yline2},{W,Yline2}]}]),
    GUI#win{wwindow=W, hwindow=Hc}.

%% Clear the GUI from applications and connecting lines
draw_clear(GUI) ->
    draw_clear2(GUI#win.appobjs),
    gs:config(GUI#win.canvas, [{hscroll, false}]),
    GUI#win{appobjs=[]}.
draw_clear2([CBtn | AppObjs]) when is_record(CBtn, canvasbutton) ->
    gs:destroy(CBtn#canvasbutton.text),
    gs:destroy(CBtn#canvasbutton.ul),
    gs:destroy(CBtn#canvasbutton.ll),
    gs:destroy(CBtn#canvasbutton.rect),
    draw_clear2(AppObjs);
draw_clear2([GSObj | AppObjs]) ->
    gs:destroy(GSObj),
    draw_clear2(AppObjs);
draw_clear2([]) ->
    ignore.

%% Display the applications, which are a list of tuples: {Pid,App,Descr}
%% Display them in the reversed order to get them chronologically
%% from left to right.
draw_apps(GUI, Apps) ->
    {AppObjs, WCanvas} = draw_apps(GUI, lists:reverse(Apps), ?xNODELBL,
				   undefined, 0, []),
    NewGUI = GUI#win{wcanvas=WCanvas, appobjs=AppObjs},
    display_scrollbar(NewGUI),
    NewGUI.

draw_apps(GUI, [App | Apps], X, Lx0, N, GSObjs) ->

    %% Some necessary data
    {_Pid, AppName, _Descr} = App,
    Text = atom_to_list(AppName),
    Width = erlang:max(8*length(Text)+10, ?wBTN),

    %% Connect the application to the node label with a line
    %% Lx0 = leftmost X coordinate (above previous application button)
    %% Lx = X coordinate, Ly1, Ly2 = top and bottom Y coordinates
    Lx = X + trunc(Width/2),
    Line = case N of
	       %% First (leftmost application) - draw a vertical line
	       %% between the node label and application button
	       0 ->
		   Ly1 = ?yNODELBL + ?hNODELBL +?PAD,
		   Ly2 = Ly1 + ?PAD2,
		   gs:create(line, GUI#win.canvas,
			     [{coords, [{Lx, Ly1}, {Lx, Ly2}]}]);
	       %% Nth application, N>1 - draw a horizontal line from
	       %% line connecting to the previous application button,
	       %% to above this application button, then vertically down
	       %% to the application button
	       _ ->
		   Ly1 = ?yNODELBL + ?hNODELBL + ?PAD + ?PAD2/2,
		   Ly2 = Ly1 + ?PAD2/2,
		   gs:create(line, GUI#win.canvas,
			     [{coords, [{Lx0, Ly1}, {Lx, Ly1},
					{Lx, Ly2}]}])
	   end,
    
    %% The application is represented using a 'canvasbutton'
    Data = {application, AppName, GUI#win.name},
    AppBtn = canvasbutton(GUI#win.canvas, Text, X, ?yBTN, Width, ?hBTN,
			  Data),

    draw_apps(GUI, Apps, X+Width+?PAD, Lx, N+1, [AppBtn, Line|GSObjs]);
draw_apps(_GUI, [], X, _N, _Lx0, GSObjs) ->
    {GSObjs, X}.

%%--Display functions---------------------------------------------------
%% Functions that modify the GUI but not its data

%% Add a new node to the Nodes menu
%% Used when a new node has connected
display_addnode([GUI|GUIs], Node) ->
    display_addnode(GUI, Node),
    display_addnode(GUIs, Node);
display_addnode([], _Node) ->
    ignore;
display_addnode(GUI, Node) ->
    Txt = "Show " ++ atom_to_list(Node),
    gs:create(menuitem, GUI#win.nodemenu,
	      [{label, {text,Txt}},
	       {data, {node, Node, GUI#win.window}}]).

%% Show that a node has come back up
display_nodeup(GUI, Node) ->
    {Rect, Text} = GUI#win.nodelabel,

    %% Check coordinates for the rectangle and compute the new width
    [{L, T}, {_R, B}] = gs:read(Rect, coords),
    NodeStr = atom_to_list(Node),
    W = 8*length(NodeStr)+10,
    
    gs:config(Rect, [{coords, [{L, T}, {L+W, B}]}, {fill, black}]),
    gs:config(Text, [{text, NodeStr}, {fg, {250,235,215}},
		     {coords,
		      [{L+round(W/2), T+round((?hNODELBL)/2)}]}]).

%% Show that a node has gone down
display_nodedown(GUI) ->
    {Rect, Text} = GUI#win.nodelabel,
    
    [{L, T}, {_R, B}] = gs:read(Rect, coords),
    gs:config(Rect, [{coords, [{L, T}, {L+114, B}]}, {fill, gray}]),
    gs:config(Text, [{text, "No connection"}, {fg, black},
		     {coords, [{L+57, T+round((?hNODELBL)/2)}]}]).

%% Add/remove scrollbars as necessary
display_scrollbar(GUI) ->

    WWindow = GUI#win.wwindow,
    HWindow = GUI#win.hwindow,
    WCanvas = GUI#win.wcanvas,
    HCanvas = GUI#win.hcanvas,
    if
	WCanvas>WWindow ->
	    gs:config(GUI#win.canvas,
		      [{hscroll, bottom},
		       {scrollregion,{0,0,WCanvas,HCanvas}}]);
	true ->
	    gs:config(GUI#win.canvas, [{hscroll, false}])
    end,
    if
	HCanvas>HWindow ->
	    gs:config(GUI#win.canvas,
		      [{vscroll, left},
		       {scrollregion,{0,0,WCanvas,HCanvas}}]);
					
	true ->
	    gs:config(GUI#win.canvas, [{vscroll, false}])
    end.

%% Select option radio buttons
display_setopt(GUI, Option) ->
    gs:config(radiobutton(GUI, Option), {select,true}).

radiobutton(GUI, single) -> (GUI#win.options)#options.single;
radiobutton(GUI, many) ->   (GUI#win.options)#options.many;
radiobutton(GUI, time) ->   (GUI#win.options)#options.time;
radiobutton(GUI, queue) ->  (GUI#win.options)#options.queue;
radiobutton(GUI, prog) ->   (GUI#win.options)#options.prog;
radiobutton(GUI, linear) -> (GUI#win.options)#options.linear.

%% Display load
%% Used when load information is received from the displayed node
-define(highloadfg, {255,99,71}).
-define(midloadfg, yellow).
-define(lowloadfg, green).
-define(highloadbg, {140,157,178}).
-define(midloadbg, ?highloadbg).
-define(lowloadbg, ?highloadbg).

display_load(GUI, {Old, New}) ->
    if
	Old == New ->
	    true;
	Old > New ->
	    leds_down(GUI#win.leds, Old, New);
	true ->
	    leds_up(GUI#win.leds, Old, New)
    end.

leds_down(_Leds, Old, New) when Old == New -> 
    done;
leds_down(Leds, Old, New) when Old > New -> 
    reset_led(Leds, Old),
    leds_down(Leds, Old-1, New).
leds_up(_Leds, Old, New) when Old == New -> 
    done;
leds_up(Leds, Old, New) when Old < New -> 
    set_led(Leds, Old),
    leds_up(Leds, Old+1, New).

led_on_col(N) when N > 13 -> ?highloadfg;
led_on_col(N) when N > 9 -> ?midloadfg;
led_on_col(_) -> ?lowloadfg.

led_off_col(N) when N > 13 -> ?highloadbg;
led_off_col(N) when N > 9 -> ?midloadbg;
led_off_col(_) -> ?lowloadbg.

reset_led(_Leds, 0) -> ok;
reset_led(Leds, N) ->
    gs:config(lists:nth(N, Leds), [{fill, led_off_col(N)}]).

set_led(_Leds, 0) -> ok;
set_led(Leds, N) ->
    gs:config(lists:nth(N, Leds), [{fill, led_on_col(N)}]).

%%----------------------------------------------------------------------
%% Utilities
%%----------------------------------------------------------------------

bcast(MNodes, Msg) ->
    lists:foreach(fun(MNode) ->
			  case MNode#mnode.status of
			      alive ->
				  MNode#mnode.pid ! Msg;
			      dead ->
				  ignore
			  end
		  end,
		  MNodes).

%% parse_nodes(MNodes) -> NodeApps
%%   MNodes -> [#mnode{}]
%%   NodeApps -> [{Node, Status, Apps}]
%%     Node -> atom()
%%     Status -> alive | dead
%%     Apps -> [{Pid, App}]
%%       Pid -> pid()
%%       App -> atom()
parse_nodes(MNodes) ->
    parse_nodes(MNodes, []).
parse_nodes([MNode|MNodes], NodeApps) ->
    Apps = parse_apps(MNode#mnode.apps, []),
    parse_nodes(MNodes,
		[{MNode#mnode.name,MNode#mnode.status,Apps}|NodeApps]);
parse_nodes([], NodeApps) ->
    NodeApps.
    
parse_apps([{Pid, App, _Descr}|Rest], Apps) ->
    parse_apps(Rest, [{Pid, App}|Apps]);
parse_apps([], Apps) ->
    Apps.

%%----------------------------------------------------------------------
%% Canvas buttons
%%----------------------------------------------------------------------

canvasbutton(Canvas, Text, X, Y, W, H, Data) ->

    %% Draw a rectangle (for event catching)
    Rect = gs:create(rectangle, Canvas, [{coords, [{X,Y}, {X+W,Y+H}]},
					 {fill, gs:read(Canvas, bg)},
					 {buttonpress, true},
					 {buttonrelease, true}]),
    
    %% Make the rectangle area look like a 3D button by using lines
    Ul = gs:create(line, Canvas, [{coords, [{X,Y+H},{X,Y},{X+W,Y}]},
				  {fg, white}, {width, 2}]),
    Ll = gs:create(line, Canvas, [{coords, [{X,Y+H},{X+W,Y+H},{X+W,Y}]},
				  {fg, {87,87,87}}, {width, 2}]),

    %% Write the text in the middle
    Xc = X + round(W/2),
    Yc = Y + round(H/2),
    T = gs:create(text, Canvas, [{text, Text}, {coords, [{Xc,Yc}]},
				 {anchor, c},
				 {buttonpress, true},
				 {buttonrelease, true}]),

    %% Create the canvasbutton object
    CBtn = #canvasbutton{text=T, ul=Ul, ll=Ll, rect=Rect,
			 x=X, y=Y, w=W, h=H},

    %% Configure the data
    gs:config(T, {data, {canvasbutton, CBtn, Data}}),
    gs:config(Rect, {data, {canvasbutton, CBtn, Data}}),

    CBtn.

press(Canvasbutton) ->
    gs:config(Canvasbutton#canvasbutton.ul, {fg, {87,87,87}}),
    gs:config(Canvasbutton#canvasbutton.ll, {fg, white}).

release(Canvasbutton) ->
    gs:config(Canvasbutton#canvasbutton.ul, {fg, white}),
    gs:config(Canvasbutton#canvasbutton.ll, {fg, {87,87,87}}).
