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
-module(tv_nodewin).



-export([start/2, init/3]).


-include("tv_int_msg.hrl").



-define(WINDOW_WIDTH, 230).
-define(WINDOW_HEIGHT, 260).
-define(DEFAULT_BG_COLOR, {217,217,217}).
-define(POLL_INTERVAL, 5000).



%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




start(CurrNode, ErrMsgMode) ->
    spawn_link(?MODULE, init, [self(), CurrNode, ErrMsgMode]).





init(Pid, CurrNode, ErrMsgMode) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    put(error_msg_mode, ErrMsgMode),
    gs:start(),
    NewCurrNode = update_node_listbox(CurrNode, false),
    tell_master(NewCurrNode, CurrNode, Pid),
    loop(Pid, NewCurrNode, node(), false).




%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************



init_window(CurrNode, Pid) ->
    create_window(),
    NewCurrNode = update_node_listbox(CurrNode, true),
    tell_master(NewCurrNode, CurrNode, Pid),
    gs:config(win, [{map,true}]),
    NewCurrNode.




handle_error(nodedown) ->
    gs:window(errorwin, gs:start(), []),
    gs:config(errorwin, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(errorwin, "TV Notification", ["The selected node is down!"]);
	haiku ->
	    Msg = ["With searching comes loss",
		   "And the presence of absence:",
		   "Node is down."],
	    tv_utils:notify(errorwin, "TV Notification", Msg)
    end,
    gs:destroy(errorwin);
handle_error(distributed) ->
    gs:window(errorwin, gs:start(), []),
    gs:config(errorwin, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(errorwin, "TV Notification", 
			    ["The system has become distributed!"]);
	haiku ->
	    Msg = [],
	    tv_utils:notify(errorwin, "TV Notification", Msg)
    end,
    gs:destroy(errorwin);
handle_error(undistributed) ->
    gs:window(errorwin, gs:start(), []),
    gs:config(errorwin, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(errorwin, "TV Notification", 
			    ["The system is no longer distributed!"]);
	haiku ->
	    Msg = ["The system you see",
		   "Is not a distributed",
		   "system anymore."],
	    tv_utils:notify(errorwin, "TV Notification", Msg)
    end,
    gs:destroy(errorwin).

get_node_lists(CurrNode) ->
    NodeDataList = lists:sort([node() | nodes()]),
    NodeTextList = lists:map(fun(Item) ->
					 " " ++ atom_to_list(Item)
				 end, 
				 NodeDataList),

    %% It *may* be possible that CurrNode has disappeared!
    %% If this is the case, use the node where TV resides
    %% as new current node.
    %% This also covers the case when our own node (or some
    %% other node) suddenly goes distributed.

    NewCurrNode = case lists:member(CurrNode, NodeDataList) of
		      true ->
			  CurrNode;
		      false ->
			  node()
		  end,
    
       %% Now get the index that shall be marked in the node listbox.
       %% Remember that the first item has number 0 (zero)!
    NodeMarkIndex = get_node_mark_index(NewCurrNode, NodeDataList, 0),
    
    {NewCurrNode, NodeDataList, NodeTextList, NodeMarkIndex}.
    



%% We know that CurrNode is *somewhere* in the list, since we have checked.
%% If the original CurrNode wasn't there, then we are using node() instead,
%% which definitely is in the list. (node() may have gone distributed in the 
%% meantime, but it *IS* in the list!)  :-)

get_node_mark_index(CurrNode, [H | T], Acc) when CurrNode =/= H ->
    get_node_mark_index(CurrNode, T, Acc + 1);
get_node_mark_index(CurrNode, [CurrNode | _], Acc) ->
    Acc.  %% Acc tells the index of the current head.  :-)
    




check_selected_node('nonode@nohost', _OldNode, _WinCreated) when node() =:= 'nonode@nohost' ->
       %% Not distributed, OK!
    'nonode@nohost';
check_selected_node(_Node, _OldNode, WinCreated) when node() =:= 'nonode@nohost' ->
       %% No longer distributed, but previously was!
    handle_error(undistributed),
    update_node_listbox('nonode@nohost', WinCreated);
check_selected_node(Node, _OldNode, _WinCreated) when Node =:= node() ->
       %% We are distributed, but on 
       %% our own node! Since we 
       % still are running, the node 
       %% is up.
    Node;
check_selected_node(Node, 'nonode@nohost', WinCreated) ->
       %% The system has been distributed!
    net_kernel:monitor_nodes(true),
    handle_error(distributed),
    update_node_listbox(Node, WinCreated);
check_selected_node(Node, _OldNode, WinCreated) ->
       %% We are distributed, and a new node has been chosen!
       %% We better check this node!
    case net_adm:ping(Node) of
	pong ->
	    Node;
	_Other ->
	    handle_error(nodedown),
	    update_node_listbox(Node, WinCreated)
    end.



available_nodes() ->
    lists:sort([node() | nodes()]).



loop(Pid, CurrNode, HomeNode, WinCreated) ->
    receive

	{nodedown, _Node} ->
	    flush_nodedown_messages(),
	    flush_nodeup_messages(),
	    case lists:member(CurrNode, available_nodes()) of
		true ->
		    done;
		false when node() =:= 'nonode@nohost', CurrNode =/= 'nonode@nohost' ->
		    handle_error(undistributed);
		false ->
		    handle_error(nodedown)
	    end,
	    NewCurrNode = update_node_listbox(CurrNode, WinCreated),
	    tell_master(NewCurrNode, CurrNode, Pid),
	    loop(Pid, NewCurrNode, node(), WinCreated);


	{nodeup, _Node} ->
	    flush_nodeup_messages(),
	    flush_nodedown_messages(),
	    case lists:member(CurrNode, available_nodes()) of
		true ->
		    done;
		false when node() =:= 'nonode@nohost', CurrNode =/= 'nonode@nohost' ->
		    handle_error(undistributed);
		false when CurrNode =:= 'nonode@nohost' ->
		    net_kernel:monitor_nodes(true),
		    handle_error(distributed);
		false ->
		    handle_error(nodedown)
	    end,
	    NewCurrNode = update_node_listbox(CurrNode, WinCreated),
	    tell_master(NewCurrNode, CurrNode, Pid),
	    loop(Pid, NewCurrNode, node(), WinCreated);


	{gs, node_listbox, click, Data, [Idx, _Txt | _]} ->
	    NewCurrNode = check_selected_node(lists:nth(Idx + 1, Data), CurrNode, WinCreated),
	    tell_master(NewCurrNode, CurrNode, Pid),
	    loop(Pid, NewCurrNode, node(), WinCreated);


	{gs, win, configure, _, _} ->
	    gs:config(win, [{width, ?WINDOW_WIDTH}, {height, ?WINDOW_HEIGHT}]),
	    loop(Pid, CurrNode, HomeNode, WinCreated);


	show_window when WinCreated->
	    gs:config(win, [raise]),
	    loop(Pid, CurrNode, HomeNode, WinCreated);
	
	show_window when not WinCreated ->
	    init_window(CurrNode, Pid),
	    loop(Pid, CurrNode, HomeNode, true);

	{gs, _Id, click, close_menu, _Args} ->
	    gs:destroy(win),
	    loop(Pid, CurrNode, HomeNode, false);


	{gs, _Id, keypress, _Data, [c, _, 0, 1 | _]} ->
	    gs:destroy(win),
	    loop(Pid, CurrNode, HomeNode, false);
	

	{gs, _Id, keypress, _Data, ['C', _, 1, 1 | _]} ->
	    gs:destroy(win),
	    loop(Pid, CurrNode, HomeNode, false);


	{gs, _Id, keypress, _Data, _Args} ->
	    loop(Pid, CurrNode, HomeNode, WinCreated);    
	

	{gs, _, destroy, _, _} ->
	    loop(Pid, CurrNode, HomeNode, false);    


	{error_msg_mode, Mode} ->
	    put(error_msg_mode, Mode),
	    loop(Pid, CurrNode, HomeNode, WinCreated);

	{'EXIT', Pid, _Reason} ->
	    net_kernel:monitor_nodes(false),
	    exit(normal);


	{'EXIT', _OtherPid, _Reason} ->
	    loop(Pid, CurrNode, HomeNode, WinCreated);

	
	_Other ->
	    io:format("Node window received message ~p ~n", [_Other]),
	    loop(Pid, CurrNode, HomeNode, WinCreated)

    after 
	1000 ->
	    NewHomeNode = case node() of
			      HomeNode ->
				  HomeNode;
			      Other ->
				  self() ! {nodeup, Other}
			  end,
	    loop(Pid, CurrNode, NewHomeNode, WinCreated)
    end.

			      


tell_master(NewNode, NewNode, _Pid) ->
    done;
tell_master(NewNode, _OldNode, Pid) ->
    Pid ! {tv_new_node, self(), NewNode}.




flush_nodedown_messages() ->
    receive
	{nodedown,_Node} ->
	    flush_nodedown_messages()
    after
	0 ->
	    done
    end.




flush_nodeup_messages() ->
    receive
	{nodeup,_Node} ->
	    flush_nodeup_messages()
    after
	0 ->
	    done
    end.




update_node_listbox(Node, WinCreated) ->
    {NewNode, NodeDataList, NodeTextList, MarkIndex} = get_node_lists(Node),
    case WinCreated of
	false ->
	    done;
	true ->
	    catch gs:config(node_listbox, [{data, NodeDataList},
					   {items, NodeTextList},
					   {selection, MarkIndex}
					  ])
    end,
    NewNode.





create_window() ->
    gs:window(win, gs:start(), [{width, ?WINDOW_WIDTH},
				{height, ?WINDOW_HEIGHT},
				{bg, ?DEFAULT_BG_COLOR},
				{title, "[TV]   Connected nodes"},
				{configure, true},
				{destroy, true},
				{cursor, arrow},
				{keypress, true}
			       ]),
    gs:menubar(menubar, win, [{bg, ?DEFAULT_BG_COLOR}
					]),
    gs:menubutton(mbutt, menubar, [{bg, ?DEFAULT_BG_COLOR},
				   {fg, {178, 34, 34}},  % firebrick
				   {label, {text, " File "}},
				   {underline, 1}
				  ]),
    
       % Create the actual menu!
    gs:menu(menu, mbutt, [{bg, ?DEFAULT_BG_COLOR},
			  {fg, {178, 34, 34}}]), 
    gs:menuitem(menu, [{bg, ?DEFAULT_BG_COLOR},
		       {fg, {178, 34, 34}},
		       {label, {text, " Close    Ctrl-C "}},
		       {data, close_menu},
		       {underline, 1}
		      ]),

    Xpos = 4, 
    Ypos = 40,
    gs:listbox(node_listbox, win, [{x, Xpos},
				   {y, Ypos},
				   {width, ?WINDOW_WIDTH - 2 * Xpos},
				   {height, ?WINDOW_HEIGHT - Ypos - Xpos},
				   {bg, {255,255,255}},
				   {vscroll, right},
				   {hscroll, true},
				   {click, true}
			      ]).
    







