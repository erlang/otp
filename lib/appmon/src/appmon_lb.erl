%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

%%% Due to the fact that the application buttons in the appmon window
%%% gets too small to read when the number of applications increases,
%%% this listbox window has been created. 
%%% Because of the limitations of GS a listbox was chosen to keep
%%% the nodes and applications. When it's possible to scroll a frame I
%%% think one should put in scrollbars in the appmon main window.
%%% The listbox solution is too slow with lots of applications.
%%%
%%% In the listbox the nodes are shown with their applications beneith.
%%% By double clicking on an application name, or a single click and
%%% then pressing the load button, its application window is started.

-module(appmon_lb).

-export ([
	  start/1, 
	  stop/1, 
	  add_node/2,
	  remove_node/2,
	  add_apps/3,
	  remove_app/3,
	  open_win/2,
	  update_status/3
	 ]).

-export ([init/1]).

-define (LB_W, 200).                     % List box width
-define (LB_H, 400).                     % List box height
-define (BUTT_W, 100).
-define (WIN_W, ?LB_W + ?BUTT_W + 25).   % Window width
-define (WIN_H, ?LB_H + 20).             % Window height


%%% #node{} 
%%%
%%% The record 'node' contains the name of the node, its status and 
%%% the applications running on that node.
%%%
%%% node    ==  atom ()
%%% status  ==  alive  ||  dead
%%% apps    ==  [#app{}]
%%%

-record (node, {node,             %% Name of the node
		status = alive,
		apps = []}). 

%%% #app{}
%%%
%%% The record 'app' contains the name of the application and its pid
%%%
%%% app  ==  atom ()
%%% pid  ==  pid ()
%%%

-record (app, {app,
		pid}).


%%% #win{}
%%%
%%% The record 'win' contains the pid of the listbox window,
%%% its x and y position, its width and height.
%%%
%%% pid     ==  win_closed  ||  pid ()
%%% x       ==  integer ()
%%% y       ==  integer ()
%%% width   ==  integer ()
%%% height  ==  integer ()
%%%

-record (win, {pid = win_closed,
	       x = 50, 
	       y = 50,
	       width = ?WIN_W,
	       height = ?WIN_H}).



%%% Every function in the interface is called with the pid 
%%% of this recieve loop, called 'LbPid'.
%%%


%%% start  /1
%%%
%%% start returns the pid of the spawned receive loop or 
%%% it will call exit/2 after a timeout.
%%%
%%% Pre:
%%%    CallingPid  ==  pid ()
%%%
%%% Def:
%%%    pid ()  ||  exit/2
%%%

start (CallingPid) ->
    PidInit = spawn (?MODULE, init, [CallingPid]),

    %% Wait for a initialization completion message from
    %% the spawned process before returning its Pid.

    receive
	{initialization_complete, PidInit} ->
	    PidInit
    
	    %% (Conditional) Failure to start within the time limit 
	    %% will result in termination (Timeout may be infinite).

    after
	60000 ->
	    exit (PidInit, kill),
	    exit ({startup_timeout, ?MODULE})
    end.



%%% stop  /1
%%%
%%% stop exits the receive loop
%%%
%%% Post:
%%%    exiting the receive loop
%%%

stop (LbPid) ->
    call (LbPid, stop).



%%% add_node  /2
%%%
%%% add_node adds the given node to the DB list.
%%%
%%% Pre:
%%%    Node  ==  atom ()
%%%
%%% Post:
%%%    Node is added to the DB list
%%%

add_node (LbPid, Node) ->
    call (LbPid, {add_node, Node}).



%%% remove_node  /2
%%%
%%% remove_node removes the given node from the DB list.
%%%
%%% Pre:
%%%    Node  ==  atom ()
%%%
%%% Post:
%%%    Node is removed from the DB list
%%%

remove_node (LbPid, Node) ->
    call (LbPid, {remove_node, Node}).



%%% add_apps  /3
%%%
%%% add_apps add the given applications to the given 
%%% node in the DB list.
%%%
%%% Pre:
%%%    Apps  ==  [App]
%%%    App   ==  {Name, Pid}
%%%    Name  ==  atom ()
%%%    Pid   ==  pid ()
%%%    Node  ==  atom ()
%%%
%%% Post:
%%%    Node#node{apps = Apps}
%%%

add_apps (LbPid, Apps, Node) ->
    call (LbPid, {add_apps, Apps, Node}).



%%% remove_app  /3
%%%
%%% remove_app remove the given application from the 
%%% given node in the DB list.
%%%
%%% Pre:
%%%    App   ==  atom ()
%%%    Node  ==  atom ()
%%%
%%% Def:
%%%    Node#node{apps = OldApps - App}
%%%

remove_app (LbPid, App, Node) ->
    call (LbPid, {remove_app, App, Node}).



%%% open_win  /3
%%%
%%% open_win opens the listbox window with the given nodes
%%% and their applications.
%%%
%%% Pre:
%%%    Nodes_apps  ==  [{Node, Status, Apps}]
%%%    Node        ==  atom ()
%%%    Status      ==  alive  ||  dead
%%%    Apps        ==  [App]
%%%    App         ==  {AppName, AppPid}
%%%    AppName     ==  atom ()
%%%    AppPid      ==  pid ()
%%%
%%% Post:
%%%    Window with listbox
%%%

open_win (LbPid, Nodes_apps) ->
    call (LbPid, {open_win, Nodes_apps}).



%%% update_status  /3
%%%
%%% update_status changes the status for the given node.
%%%
%%% Pre:
%%%    Node    ==  atom ()
%%%    Status  ==  alive  ||  dead
%%%
%%% Def:
%%%    Node#node{status = Status}
%%%

update_status (LbPid, Node, Status) ->
    call (LbPid, {update_status, Node, Status}).



%%% call  /2
%%%
%%% call sends the given action to the listbox receive loop.
%%%
%%% Pre: 
%%%    Action  ==  atom ()  ||  tuple ()
%%%

call (LbPid, Action) ->
    LbPid ! Action. 
 


%%% init  /1
%%%

init (CallingPid) ->
    CallingPid ! {initialization_complete, self ()},
    loop (#win{}, []).



%%% loop  /2
%%%
%%% loop is the recive loop for the listbox window process.
%%%
%%% Pre:
%%%    Win   ==  #win{}
%%%    Data  ==  [#node{}]
%%%

loop (Win, Data) ->
    receive
	{add_node, Node} ->
	    NewData = add_node_1 (Node, Data),
	    update (NewData, Win#win.pid),
	    loop (Win, NewData);
	
	{remove_node, Node} ->
	    NewData = dead_node (Node, Data),
	    update (NewData, Win#win.pid),
	    loop (Win, NewData);

	{add_apps, Apps, Node} ->
	    NewData = add_apps_1 (Apps, Node, Data),
	    update (NewData, Win#win.pid),
	    loop (Win, NewData);
	
	{remove_app, App, Node} ->
	    NewData = remove_app_1 (App, Node, Data),
	    update (NewData, Win#win.pid),
	    loop (Win, NewData);
	
	{open_win, Nodes_apps} ->
	    NewData = parse_data ([], Nodes_apps),
	    NewWin = Win#win{pid = init_win ({Win#win.x, Win#win.y})},
	    update (NewData, NewWin#win.pid),
	    loop (NewWin, NewData);
	    
	{update_status, Node, Status} ->
	    NewData = update_status_1 (Node, Status, Data),
	    update (NewData, Win#win.pid),
	    loop (Win, NewData);
	
	stop ->
	    true;


	{gs, _Id, destroy, _D, _Arg} -> 
	    bye;

	{gs, _Id, configure, _D, [W, H | _]} ->
	    NewWin = configure (Win#win.pid, W, H),
	    loop (NewWin, Data);

	{gs, lb, doubleclick, _, _Txt} ->
	    load_app (gs:read (lb, selection), Data),
	    loop (Win, Data);
	    
	{gs, lb, click, _, _Txt} ->
	    loop (Win, Data);

	{gs, close, click, _D, _Arg} -> 
	    case Win#win.pid of
		win_closed ->
		    true;
		
		_opened ->
		    gs:destroy (Win#win.pid)
	    end,
	    loop (#win{}, Data);

	{gs, load, click, _D, _Txt} ->
	    load_app (gs:read (lb, selection), Data),
	    loop (Win, Data);
	
	{gs, clear, click, _D, _Txt} ->
	    gs:config (lb, {selection, clear}),
	    loop (Win, Data);
	

	_ ->
	    loop (Win, Data)
    end.



%%% init_win  /1
%%%

init_win ({X, Y}) ->
    GS = gs:start (),

    Win = gs:window (win, GS, [{x, X},
			       {y, Y},
			       {width, ?WIN_W},
			       {height, ?WIN_H},
			       {title,"Appmon: nodes and applications"},
			       {configure, true}]),

    gs:listbox (lb, Win, [{x, 5},
			  {y, 10},
			  {width, ?LB_W},
			  {height, ?LB_H},
			  {vscroll, right},
			  {hscroll, bottom},
			  {selectmode, single},
			  {click, true},
			  {doubleclick, true}]),

    gs:button (load, Win, [{x, ?WIN_W - ?BUTT_W - 10},
			    {y, ?WIN_H - 120},
			    {width, ?BUTT_W},
			    {label, {text, "Load"}}]),

    gs:button (clear, Win, [{x, ?WIN_W - ?BUTT_W - 10},
			    {y, ?WIN_H - 80},
			    {width, ?BUTT_W},
			    {label, {text, "Clear"}}]),

    gs:button (close, Win, [{x, ?WIN_W - ?BUTT_W - 10},
			   {y, ?WIN_H - 40},
			   {width, ?BUTT_W},
			   {label, {text, "Close"}}]),

    gs:config (Win, {map, true}),
    Win.


			  			 
%%% add_node_1  /2
%%%
%%% add_node adds the given node in the given window 
%%% with its appications in a listbox.
%%%

add_node_1 (Node, []) ->
    [new_node (Node)];

add_node_1 (Node, [H | T]) ->
    T1 = lists:keysort (#node.node, [new_node (Node) | T]),
    [H | T1].



%%% dead_node  /2
%%%
%%% dead_node returns a list with the given node's 
%%% status changed to dead.
%%%

dead_node (Node, Data) ->
    case lists:keysearch (Node, #node.node, Data) of
	{value, Node_rec} ->
	    L = Node_rec#node.apps,
	    lists:keyreplace (Node, #node.node, 
			      Data, new_node (Node, dead, L));
	
	_false ->
	    Data
    end.

    



%%% add_apps_1  /3
%%%
%%% add_apps_1 returns a list with the given application 
%%% into the old list inserted.
%%%

add_apps_1 (Apps, Node, Data) ->
    case lists:keysearch (Node, #node.node, Data) of
	{value, _Node_rec} ->
	    NewApps = parse_apps (Apps, []),
	    lists:keyreplace (Node, #node.node, 
			      Data, new_node (Node, NewApps));
	
	_false ->
	    Data
    end.



%%% remove_app_1  /3
%%%
%%% remove_app_1 returns a list with the given application
%%% removed from the old list.
%%%

remove_app_1 (App, Node, Data) ->
    
    case lists:keysearch (Node, #node.node, Data) of
	{value, Node_rec} ->
	    L = Node_rec#node.apps,
	    L2 = lists:keydelete (App, #app.app, L),
	    lists:keyreplace(Node, #node.node, Data, new_node(Node,L2));
	
	_false ->
	    Data
    end.



%%% configure  /3
%%%
%%% configure returns a win record after the window has been
%%% configured.
%%%

configure (WPid, W, H) ->
    X = gs:read (WPid, x),
    Y = gs:read (WPid, y),

    gs:config (lb, [{width, W - ?BUTT_W - 25}, {height, H - 20}]),
    gs:config (load, [{x, W - ?BUTT_W - 10}, {y, H - 120}]),
    gs:config (clear, [{x, W - ?BUTT_W - 10}, {y, H - 80}]),
    gs:config (close, [{x, W - ?BUTT_W - 10}, {y, H - 40}]),
    
    #win{pid = WPid, x = X, y = Y, width = W, height = H}.

    



%%% load_app  /2
%%%
%%% load_app opens the application window by calling 
%%% the appmon_a module.
%%%

load_app ([], _Data) ->     %% no application chosen
    ok;

load_app ([Index], Data) ->
    App = gs:read (lb, {get, Index}),
    
    case string:substr (App, 1, 3) of
	"   " ->
	    AppName = list_to_atom (string:substr (App, 4)),
	    
	    case get_node (AppName, Index, Data) of
		no_node ->
		    ok;

		NodeName ->
		    appmon_a:start (NodeName, AppName)
	    end;

	_ ->
	    ok
    end.



%%% update_status_1  /3
%%%
%%% update_status_1 returns a list with the given 
%%% node's status updated.
%%%

update_status_1 (Node, Status, Data) ->
    case lists:keysearch (Node, #node.node, Data) of
	{value, Node_rec} ->
	    lists:keyreplace (Node, 
			      #node.node, 
			      Data, 
			      new_node(Node,Status,Node_rec#node.apps));

	_not_found ->
	    Data
    end.



%%% update  /2
%%%
%%% update updates the listbox with new data.
%%%

update (_Data, win_closed) ->
    true;

update (Data, _Win) ->
    gs:config (lb, clear),
    lb_print (Data).



%%% lb_print  /1
%%%
%%% lb_print prints the list into the listbox.
%%%

lb_print ([]) ->
    ok;

lb_print ([#node{node = Node, status = Status, apps = Apps} | T]) ->
    Str = io_lib:format ("~p (~p)", [Node, Status]),
    gs:config (lb, {add, Str}),

    case Status of
	alive ->
	    lb_print_apps (Apps);

	_dead ->
	    gs:config (lb, {add, ""}),
	    ok
    end,

    lb_print (T).



%%% lb_print_apps  /1
%%%
%%% lb_print_apps prints the applications into the listbox.
%%%

lb_print_apps ([]) ->
    ok;

lb_print_apps ([#app{app = App} | T]) ->
    Str = io_lib:format ("   ~p", [App]),
    gs:config (lb, {add, Str}),
    lb_print_apps (T).



%%% new_node  /1, 2, 3
%%%
%%% new_node returna a new node record constructed 
%%% with the given data
%%%

new_node (Node) ->
    #node{node = Node}.

new_node (Node, Apps) ->
    #node{node = Node, apps = Apps}.

new_node (Node, Status, Apps) ->
    #node{node = Node, status = Status, apps = Apps}.



%%% new_app  /2
%%%
%%% new_app returns a new application record 
%%% constructed with the given data.
%%%

new_app (App, Pid) ->
    #app{app = App, pid = Pid}.



%%% parse_apps  /2
%%%
%%% parse_apps returns a list of application records.
%%%

parse_apps ([], [H | T]) ->
    [H | lists:keysort (#app.app, T)];

parse_apps ([App | T], L) ->
    Pid = element (1, App),
    Name = element (2, App),
    parse_apps (T, [new_app (Name, Pid) | L]). 



%%% get_node  /3
%%%
%%% get_node returns the node from the given list 
%%% or else no_node if it doesn't exists.
%%%

get_node (_App, _Index, []) ->
    no_node;

get_node (App, Index, [Node | T]) ->
    Length = length (Node#node.apps) + 1,

    case Length < Index of
	true ->
	    get_node (App, Index - Length, T);

	false ->
	    Node#node.node
    end.



%%% parse_data  /2
%%%
%%% parse_data returns a list with node records.
%%%

parse_data (Data, []) ->
    Data;

parse_data (Data, [{Node, Status, Apps} | T]) ->
    Apps_1 = parse_apps (Apps, []),
    parse_data ([new_node (Node, Status, Apps_1) | Data], T).

    

