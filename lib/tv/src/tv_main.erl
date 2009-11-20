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
-module(tv_main).



-export([start/0,
	 init/0
	]).


-export([get_ets_tables/1,
	 get_mnesia_tables/1
	]).



-include("tv_main.hrl").
-include("tv_int_msg.hrl").
-include("tv_pd_int_msg.hrl").
-include("tv_pd_int_def.hrl").




%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************


start() ->
    spawn(?MODULE, init, []).



init() ->
    process_flag(trap_exit,true),
       %% OK, so it's *BAD* to use the process dictionary...
       %% So why have I used it? Because it is simple to remove the haiku-functionality,
       %% if that is desired. Otherwise a lot of functions (the parameters) would have 
       %% to be changed.
    put(error_msg_mode, ?ERROR_MSG_MODE),
    KindOfTable  = ets,
    SysTabHidden = true,
    UnreadHidden = true,
    SortKey      = ?NAME_COL,
    CurrNode     = node(),
    Children     = start_tv_nodewin(CurrNode),
    {MarkedCell, TempGridLines, WinSize, ShortcutList} = create_window([]),
    Tables       = get_tables(CurrNode, KindOfTable, UnreadHidden, SysTabHidden,SortKey),
    gs:config(grid, [{rows, {1, get_nof_rows(length(Tables), 
					     gs:read(grid, height))}}]),
    GridLines = update_gridlines(Tables, TempGridLines, 1),
    gs:config(win, [{map, true}, {cursor,arrow}]),
       %% To avoid unpleasant error/exit messages, we surround the loop with a catch.
    catch loop(KindOfTable, CurrNode, MarkedCell, GridLines, WinSize, Tables, ShortcutList,
	       UnreadHidden, SysTabHidden, SortKey, Children).



start_tv_nodewin(CurrNode) ->
    NodewinPid = tv_nodewin:start(CurrNode, get(error_msg_mode)),
    [{NodewinPid, tv_nodewin, CurrNode}].
    


    
    
get_ets_tables(SysTabHidden) ->
    Tables = ets:all(),
    get_ets_table_info(Tables, 
		       hidden_tables(ets, SysTabHidden) ++ 
		       current_mnesia_tables(SysTabHidden),
		       owners_to_hide(ets, SysTabHidden),
		       []).
    
    

get_mnesia_tables(SysTabHidden) ->
    Tables = mnesia:system_info(tables),
    get_mnesia_table_info(Tables -- hidden_tables(mnesia, SysTabHidden), 
			  owners_to_hide(mnesia, SysTabHidden),
			  []).
    




owners_to_hide(ets, true) ->
    ?SYSTEM_OWNERS;
owners_to_hide(ets, false) ->
    [];
owners_to_hide(mnesia, true) ->
    [];
owners_to_hide(mnesia, false) ->
    [].




get_mnesia_table_info([], _OwnersToHide, Acc) ->
    lists:keysort(?NAME_ELEM, Acc);
get_mnesia_table_info([TabId | Tail], OwnersToHide, Acc) ->
    case catch get_mnesia_owner_size(TabId) of
	{'EXIT', _Reason} ->  
	       %% Ignore tables ceasing to exist. 
	       %% Nodedown errors caught above!
	    get_mnesia_table_info(Tail, OwnersToHide, Acc);
	{OwnerPid, OwnerName, Size} ->
	    case lists:member(OwnerName, OwnersToHide) of
		true ->
		    get_mnesia_table_info(Tail, OwnersToHide, Acc);
		false ->
		    Readable = not(lists:member(TabId, ?UNREADABLE_MNESIA_TABLES)),
		    get_mnesia_table_info(Tail, 
					  OwnersToHide,
					  [{TabId, {notext}, {notext}, Readable, 
					    OwnerPid, OwnerName, Size} | Acc])
	    end
    end.




get_mnesia_owner_size(TabId) ->
    {OwnerPid, OwnerName} = 
	case catch mnesia:table_info(TabId, owner) of
	    Pid when is_pid(Pid) ->
		case lists:keysearch(registered_name, 1, process_info(Pid)) of
		    false ->
			{Pid, {notext}};
		    {value, {registered_name, ProcName}} ->
			{Pid, ProcName}
		end;
	    _Other ->
		{{notext}, {notext}}
	end,
    Size = mnesia:table_info(TabId, size),
    {OwnerPid, OwnerName, Size}.







hidden_tables(_Any, true) ->
    ?SYSTEM_TABLES ++ ?MNESIA_TABLES;
hidden_tables(ets, _SysTabHidden) ->
    ?MNESIA_TABLES;
hidden_tables(mnesia, _SysTabHidden) ->
    [].




get_tables(Node, KindOfTable, UnreadHidden, SysTabHidden,SortKey) ->
    LocalNode = (Node =:= node()),
    Tables = 
	case catch get_table_list(Node,LocalNode,KindOfTable,SysTabHidden) of
	    Result when is_list(Result) ->
		case UnreadHidden of 
		    true ->
			lists:filter(fun(H) ->
					     element(?READABLE_ELEM, H)
				     end, 
				     Result);
		    _Other ->
			Result
		end;
	    Error ->
		analyze_error(Error, Node, undefined),
		[]
	end,
    case SortKey of
	?PROCNAME_ELEM ->
	    lists:keysort(SortKey, 
			  lists:keysort(?PID_ELEM, Tables));
	_OtherCol ->
	    lists:keysort(SortKey, 
			  lists:keysort(?NAME_ELEM, Tables))
    end.

        



get_ets_table_info([], _TablesToHide, _OwnersToHide, Acc) ->
    lists:keysort(?ID_ELEM, Acc);
get_ets_table_info([TabId | Tail], TablesToHide, OwnersToHide, Acc) ->
    case catch get_ets_name_owner_protection(TabId) of
	{'EXIT', _Reason} ->  
	       %% Ignore tables ceasing to exist. 
	       %% Nodedown errors caught above!
	    get_ets_table_info(Tail, TablesToHide, OwnersToHide, Acc);
	{Name, NamedTable, Id, Readable, OwnerPid, OwnerName, Size} ->
	    case lists:member(Name, TablesToHide) of
		true ->
		    get_ets_table_info(Tail, TablesToHide, OwnersToHide, Acc);
		false ->
		    case lists:member(OwnerName, OwnersToHide) of
			true ->
			    get_ets_table_info(Tail, TablesToHide, OwnersToHide, Acc);
			false ->
			    get_ets_table_info(Tail, TablesToHide, OwnersToHide,
					       [{Name,NamedTable,Id,Readable,
						 OwnerPid,OwnerName,Size} | Acc])
		    end
	    end
    end.



get_ets_name_owner_protection(TabId) ->
    Name       = ets:info(TabId, name),
    OwnerPid   = ets:info(TabId, owner),
    Readable   = case ets:info(TabId, protection) of
		     private ->
			 false;
		     _Other ->
			 true
		 end,
    Size            = ets:info(TabId, size),
    {NamedTable,Id} = case ets:info(TabId, named_table) of
			  true ->
			      {true,{notext}};
			  false ->
			      {false, TabId}
		      end,
    PName      = case lists:keysearch(registered_name, 1, process_info(OwnerPid)) of
		     false ->
			 {notext};
		     {value, {registered_name, ProcName}} ->
			 ProcName
		 end,
    {Name, NamedTable, Id, Readable, OwnerPid, PName, Size}.

    




current_mnesia_tables(SysTabHidden) ->
    case catch get_table_list(node(), true, mnesia, SysTabHidden) of
	Result when is_list(Result) ->
	    lists:map(fun(H) ->
			      element(?NAME_ELEM, H)
		      end,
		      Result);
	nodedown ->
	    handle_error(nodedown, node(), undefined),
	    [];
	_Other ->
	    []
    end.
	    



get_table_list(_Node, true, ets, SysTabHidden) ->
    get_ets_tables(SysTabHidden);
get_table_list(Node, false, ets, SysTabHidden) ->
    case rpc:block_call(Node, ?MODULE, get_ets_tables, [SysTabHidden]) of
	{badrpc, Reason} ->
	    throw({badrpc,Reason});
	Result ->
	    Result
    end;
get_table_list(_Node, true, mnesia, SysTabHidden) ->
    get_mnesia_tables(SysTabHidden);
get_table_list(Node, false, mnesia, SysTabHidden) ->
    case rpc:block_call(Node, ?MODULE, get_mnesia_tables, [SysTabHidden]) of
	{badrpc,Reason} ->
	    throw({badrpc,Reason});
	Result ->
	    Result
    end.




analyze_error(Cause, Node, Table) ->
    case Cause of
	{badrpc, {'EXIT', {badarg,_Reason}}} ->
	    done;  %% Table has ceased to exist.
	{'EXIT', {badarg, {ets,local_info,_Args}}} ->  
	    done;
	
	{badrpc, nodedown} ->
            handle_error(nodedown, Node, Table);
	{'EXIT', nodedown} ->
            handle_error(nodedown, Node, Table);

        {'EXIT', {aborted, {node_not_running,_ErrNode}}} ->
            handle_error(mnesia_not_started, Node, Table);
        {'EXIT', {'EXIT', {aborted, {node_not_running,_ErrNode}}}} ->
            handle_error(mnesia_not_started, Node, Table);
        {badrpc, {'EXIT', {aborted, {node_not_running,_ErrNode}}}} ->
            handle_error(mnesia_not_started, Node, Table);
	{'EXIT', {undef, {mnesia,_Fcn,_Args}}} ->
	    handle_error(mnesia_not_started, Node, Table);

        {'EXIT', Reason} ->
            handle_error({unexpected_error,Reason}, Node, Table);
        Error when is_tuple(Error) ->
            handle_error({unexpected_error,Error}, Node, Table)
    end.
    
	    

handle_error(mnesia_not_started, _Node, _Table) ->
    gs:config(win, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(win, "TV Notification", ["Mnesia not started!"]);
	haiku ->
	    tv_utils:notify(win, "TV Notification", ["Mnesia is stopped.",
						     "We wish to reach all data",
						     "But we never will."])
    end;
handle_error(nodedown, _Node, _Table) ->
    gs:config(win, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(win, "TV Notification", ["The selected node is down!"]);
	haiku ->
	    Msg = ["With searching comes loss",
		   "And the presence of absence:",
		   "Node is down."],
	    tv_utils:notify(win, "TV Notification", Msg)
    end,
    self() ! nodedown;
handle_error({unexpected_error,Cause}, _Node, _Table) ->
    io:format("Unexpected error:  ~p~n", [Cause]),
    gs:config(win, [beep]).




loop(KindOfTable,CurrNode,MarkedCell,GridLines,
     WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    receive

	{gs, Gridline, click, {grid,Readable}, [Col,Row,Text | _]} when Text =/= "" ->
	    unmark_cell(MarkedCell, Tables),
	    NewMarkedCell = mark_cell({Gridline, Col, Row}, MarkedCell, Readable),
	    loop(KindOfTable,CurrNode,NewMarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, _Gridline, click, {grid,_Readable}, [_Col,_Row,"" | _]} ->
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    loop(KindOfTable,CurrNode,NewMarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, Gridline, doubleclick, {grid,Data}, [?NAME_COL,Row,Text | _]} when Text =/= "" ->
	    unmark_cell(MarkedCell, Tables),	    
	    NewMarkedCell = mark_cell({Gridline, ?NAME_COL, Row}, undefined, Data),
	    {Table, Name, Readable} = get_table_id(KindOfTable, Row, Tables),
	    case start_tv_browser(Table,CurrNode,Name,KindOfTable,Readable,Children) of
		Children ->
		    {FinalMarkedCell, NewTables, NewGridLines} = 
			refresh_window(NewMarkedCell,Tables,KindOfTable,CurrNode,GridLines,
				       UnreadHidden,SysTabHidden,SortKey, Children),
		    loop(KindOfTable,CurrNode,FinalMarkedCell,NewGridLines,WinSize,NewTables,
			 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		NewChildren ->
		    loop(KindOfTable,CurrNode,NewMarkedCell,GridLines,WinSize,Tables,Shortcuts,
			 UnreadHidden,SysTabHidden,SortKey,NewChildren)
	    end;
	    

	{gs, Gridline, doubleclick, {grid,Data}, [?ID_COL,Row,Text | _]} when Text =/= "" ->
	    unmark_cell(MarkedCell, Tables),	    
	    NewMarkedCell = mark_cell({Gridline, ?ID_COL, Row}, undefined, Data),
	    {Table, Name, Readable} = get_table_id(KindOfTable, Row, Tables),
	    case start_tv_browser(Table,CurrNode,Name,KindOfTable,Readable,Children) of
		Children ->
		    {FinalMarkedCell, NewTables, NewGridLines} = 
			refresh_window(NewMarkedCell,Tables,KindOfTable,CurrNode,GridLines,
				       UnreadHidden,SysTabHidden,SortKey, Children),
		    loop(KindOfTable,CurrNode,FinalMarkedCell,NewGridLines,WinSize,NewTables,
			 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		NewChildren ->
		    loop(KindOfTable,CurrNode,NewMarkedCell,GridLines,WinSize,Tables,Shortcuts,
			 UnreadHidden,SysTabHidden,SortKey,NewChildren)
	    end;
	    

	{gs, Gridline, doubleclick, {grid,Data}, [?INFO_COL,Row,Text | _]} when Text =/= "" ->
	    unmark_cell(MarkedCell, Tables),	    
	    NewMarkedCell = mark_cell({Gridline, ?INFO_COL, Row}, undefined, Data),
	    {Table, _Name, _Readable} = get_table_id(KindOfTable, Row, Tables),
	    case start_tv_info(Table, CurrNode, CurrNode =:= node(), KindOfTable, Children) of
		Children ->
		    {FinalMarkedCell, NewTables, NewGridLines} = 
			refresh_window(NewMarkedCell,Tables,KindOfTable,CurrNode,GridLines,
				       UnreadHidden,SysTabHidden,SortKey, Children),
		    loop(KindOfTable,CurrNode,FinalMarkedCell,NewGridLines,WinSize,NewTables,
			 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		NewChildren ->
		    loop(KindOfTable,CurrNode,NewMarkedCell,GridLines,WinSize,Tables,Shortcuts,
			 UnreadHidden,SysTabHidden,SortKey,NewChildren)
	    end;


	{gs, Gridline, doubleclick, {grid,Data}, [?PID_COL,Row,Text | _]} when Text =/= "" ->
	    unmark_cell(MarkedCell, Tables),	    
	    NewMarkedCell = mark_cell({Gridline, ?PID_COL, Row}, undefined, Data),
	    OwnerPid = element(?PID_ELEM, lists:nth(Row, Tables)),
	    NewChildren = start_pman(OwnerPid, Children),
	    loop(KindOfTable,CurrNode,NewMarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey, NewChildren);


	{gs, Gridline, doubleclick, {grid,Data}, [?PROCNAME_COL,Row,Text | _]} when Text =/= "" ->
	    unmark_cell(MarkedCell, Tables),	    
	    NewMarkedCell = mark_cell({Gridline, ?PROCNAME_COL, Row}, undefined, Data),
	    OwnerPid = element(?PID_ELEM, lists:nth(Row, Tables)),
	    NewChildren = start_pman(OwnerPid, Children),
	    loop(KindOfTable,CurrNode,NewMarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey, NewChildren);


%%	{gs, win, configure, _Data, [Width, Height | _]} when {Width,Height} /= WinSize ->
	Msg0 = {gs, win, configure, _Data, [Width0, Height0 | _]} 
	when {Width0,Height0} =/= WinSize ->
	    {gs, win, configure, _, [Width,Height|_]} = flush_msgs(Msg0),

	    NewSize = resize_window(Width, Height, length(Tables)),
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,NewSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);
	

	{gs, _Id, click, update, _Args} ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable,CurrNode,GridLines,UnreadHidden,SysTabHidden,SortKey),
	    update_tv_info(Children),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, _Id, click, open_table, _Args} ->
	    {Table, Name, Readable} = get_table_id(KindOfTable, element(3, MarkedCell), 
						   Tables),
	    case start_tv_browser(Table,CurrNode,Name,KindOfTable,Readable,Children) of
		Children ->
		    {NewMarkedCell, NewTables, NewGridLines} = 
			refresh_window(MarkedCell,Tables,KindOfTable,CurrNode,GridLines,
				       UnreadHidden,SysTabHidden,SortKey, Children),
		    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,
			 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		NewChildren ->
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
			 UnreadHidden,SysTabHidden,SortKey,NewChildren)
	    end;
	

	{gs, _Id, click, new_table, _Args} ->
	    NewChildren = start_tv_new_table(CurrNode, Children),
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,NewChildren);
	

	{gs, _Id, click, select_node, _Args} ->
	    show_tv_nodewin(Children),
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, _Id, click, show_mnesia, _Args} when KindOfTable =:= ets ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    gs:config(label2, [{fg, ?DISABLED_COLOR}]),
	    gs:config(sort_table_id, [{enable, false}]),
	    NewSortKey = 
		case SortKey of
		    ?ID_ELEM ->
			gs:config(sort_table_name, [{select,true}]),
			?NAME_ELEM;
		    _Other ->
			SortKey
		end,
	    {NewTables, NewGridLines} = 
		update_grid(mnesia, CurrNode, GridLines, UnreadHidden, SysTabHidden, NewSortKey),
	    gs:config(win, [{cursor,arrow}]),
	    loop(mnesia,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,NewSortKey,Children);


	{gs, _Id, click, show_ets, _Args} when KindOfTable =:= mnesia ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    gs:config(label2, [{fg, ?NORMAL_FG_COLOR}]),
	    gs:config(label3, [{fg, ?NORMAL_FG_COLOR}]),
	    gs:config(label4, [{fg, ?NORMAL_FG_COLOR}]),
	    {NewTables, NewGridLines} = 
		update_grid(ets, CurrNode, GridLines, UnreadHidden, SysTabHidden,SortKey),
	    %% gs:config(show_unreadable, [{enable, true},
	    %%				   {select, not(UnreadHidden)}]),
	    gs:config(sort_table_id, [{enable, true}]),
	    gs:config(win, [{cursor,arrow}]),
	    loop(ets,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);
	    

	{gs, _Id, click, show_system, _Args} when SysTabHidden ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} 
		= update_grid(KindOfTable, CurrNode, GridLines, UnreadHidden, false, SortKey),
	    gs:config(show_system, [{data, hide_system}]),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,false,SortKey,Children);


	{gs, _Id, click, hide_system, _Args} when not SysTabHidden ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable, CurrNode, GridLines, UnreadHidden, true, SortKey),
	    gs:config(show_system, [{label, {text, " System Tables "}},
				    {data, show_system}]),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,true,SortKey,Children);


	{gs, _Id, click, show_unreadable, _Args} when UnreadHidden ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} 
		= update_grid(KindOfTable, CurrNode, GridLines, false, SysTabHidden, SortKey),
	    gs:config(show_unreadable, [{data, hide_unreadable}]),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 false,SysTabHidden,SortKey,Children);


	{gs, _Id, click, hide_unreadable, _Args} when not UnreadHidden ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable, CurrNode, GridLines, true, SysTabHidden, SortKey),
	    gs:config(show_unreadable, [{label, {text, " Unreadable Tables "}},
					{data, show_unreadable}]),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 true,SysTabHidden,SortKey,Children);


	{gs, _Id, click, show_info, _Args} ->
	    {Table, _Name, _Readable} = get_table_id(KindOfTable, element(3,MarkedCell),
					      Tables),
	    case start_tv_info(Table, CurrNode, CurrNode =:= node(), KindOfTable, Children) of
		Children ->
		    {NewMarkedCell, NewTables, NewGridLines} = 
			refresh_window(MarkedCell,Tables,KindOfTable,CurrNode,GridLines,
				       UnreadHidden,SysTabHidden,SortKey, Children),
		    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,
			 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		NewChildren ->
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
			 UnreadHidden,SysTabHidden,SortKey,NewChildren)
	    end;


	{gs, _Id, click, sort_table_name, _Args} when SortKey =/= ?NAME_ELEM ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable,CurrNode,GridLines,UnreadHidden,SysTabHidden,?NAME_ELEM),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,?NAME_ELEM,Children);
	

	{gs, _Id, click, sort_table_id, _Args} when SortKey =/= ?ID_ELEM ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable,CurrNode,GridLines,UnreadHidden,SysTabHidden,?ID_ELEM),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,?ID_ELEM,Children);
		    

	{gs, _Id, click, sort_owner_name, _Args} when SortKey =/= ?PROCNAME_ELEM ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable,CurrNode,GridLines,UnreadHidden,SysTabHidden,
			    ?PROCNAME_ELEM),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,?PROCNAME_ELEM,Children);
	

	{gs, _Id, click, sort_owner_pid, _Args} when SortKey =/= ?PID_ELEM ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable,CurrNode,GridLines,UnreadHidden,SysTabHidden,?PID_ELEM),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,?PID_ELEM,Children);
	
	    
	{gs, _Id, click, trace_process, _Args} ->
	    OwnerPid = element(?PID_ELEM, lists:nth(element(3,MarkedCell), Tables)),
	    NewChildren = start_pman(OwnerPid, Children),
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,NewChildren);
	    

	{gs, _Id, click, help_button, _Args} ->
	    HelpFile = filename:join([code:lib_dir(tv), "doc", "html", "index.html"]),
	    tool_utils:open_help(win, HelpFile),
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, _Id, click, otp_help_button, _Args} ->
	    IndexFile = filename:join([code:root_dir(), "doc", "index.html"]),
	    tool_utils:open_help(win, IndexFile),
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, win, configure, _Data, _Args} ->
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, _Id, click, exit_button, _Args} ->
	    lists:foreach(
	      fun({Pid,pman,_OP}) ->
		      exit(Pid,kill);
		 (_) ->
		      done
	      end, 
	      Children),
	    exit(normal);


	{gs, _Id, click, show_haiku, _Args} ->
	    gs:config(win, [{cursor,busy}]),
	    gs:config(show_haiku, [{data, hide_haiku}]),
	    lists:foreach(
	      fun({Pid,tv_info,_Data}) ->
		      Pid ! {error_msg_mode,haiku};
		 ({Pid,tv_browser,_Data}) ->
		      Pid ! {error_msg_mode,haiku};
		 ({Pid,tv_nodewin,_Data}) ->
		      Pid ! {error_msg_mode,haiku};
		 ({Pid,tv_new_table,_Data}) ->
		      Pid ! {error_msg_mode,haiku};
		 (_Other) ->
		      done
	      end,
	      Children),
	    put(error_msg_mode, haiku),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable, CurrNode, MarkedCell, GridLines, WinSize, Tables, Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);
		

	{gs, _Id, click, hide_haiku, _Args} ->
	    gs:config(win, [{cursor,busy}]),
	    gs:config(show_haiku, [{data, show_haiku}]),
	    lists:foreach(
	      fun({Pid,tv_info,_Data}) ->
		      Pid ! {error_msg_mode,normal};
		 ({Pid,tv_browser,_Data}) ->
		      Pid ! {error_msg_mode,normal};
		 ({Pid,tv_nodewin,_Data}) ->
		      Pid ! {error_msg_mode,normal};
		 ({Pid,tv_new_table,_Data}) ->
		      Pid ! {error_msg_mode,normal};
		 (_Other) ->
		      done
	      end,
	      Children),
	    put(error_msg_mode, normal),
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable, CurrNode, MarkedCell, GridLines, WinSize, Tables, Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children);


	{gs, win, destroy, _Data, _Args} ->
	    lists:foreach(
	      fun({Pid,pman,_OP}) ->
		      exit(Pid,kill);
		 (_) ->
		      done
	      end, 
	      Children),
	    exit(normal);


	{gs, win, keypress, _Data, [Key, _, _, 1 | _]} ->
	    case lists:keysearch(Key, 1, Shortcuts) of
		{value, {Key, Value}} ->
		    handle_keypress(Value,KindOfTable,CurrNode,MarkedCell,
				    GridLines,WinSize,Tables, Shortcuts,
				    UnreadHidden,SysTabHidden,SortKey,Children);
		false ->
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
			 UnreadHidden,SysTabHidden,SortKey,Children)
	    end;


	{gs, win, keypress, _Data, _Args} ->
	    loop(KindOfTable, CurrNode, MarkedCell, GridLines, WinSize, Tables, Shortcuts,
		UnreadHidden,SysTabHidden,SortKey,Children);


	{tv_new_node, _Sender, NewCurrNode} ->
	    gs:config(win, [{cursor,busy}]),
	    NewMarkedCell = unmark_cell(MarkedCell, Tables),
	    {NewTables, NewGridLines} = 
		update_grid(KindOfTable,NewCurrNode,GridLines,UnreadHidden,SysTabHidden,SortKey),
	    update_tv_info(Children),
	    update_tv_browser(Children),
	    NewChildren = 
		case replace_node_name(NewCurrNode, CurrNode) of
		    false ->
			Children;
		    true ->
			update_node_name(Children)
		end,
	    gs:config(win, [{cursor,arrow}]),
	    loop(KindOfTable,NewCurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,NewChildren);


	{tv_start_infowin, Table, Node, LocalNode, TableType} ->
	    case start_tv_info(Table, Node, LocalNode, TableType, Children) of
		Children ->
		    {NewMarkedCell, NewTables, NewGridLines} = 
			refresh_window(MarkedCell,Tables,KindOfTable,CurrNode,GridLines,
				       UnreadHidden,SysTabHidden,SortKey, Children),
		    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,
			 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		NewChildren ->
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
			 UnreadHidden,SysTabHidden,SortKey,NewChildren)
	    end;


	{tv_update_infowin, Table, Node, _Type} ->
	    case get_tv_info_pid(Table, Node, Children) of
		undefined ->
		    done;
		Pid ->
		    Pid ! #info_update_table_info{sender=self()}
	    end,
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize, 
		 Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
	    

	{tv_new_table, NewTabWinPid, Node, Name, Options, KindOfTableToCreate, _Readable, false} ->
	    case create_table(KindOfTableToCreate, Node, Node =:= node(), Name, Options, 
			      NewTabWinPid) of
		error ->
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize, 
			 Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		_TabId ->
		    case KindOfTable of
			mnesia ->
			    done;
			ets ->
			    self() ! {gs, tv_main, click, update, []}
		    end,
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,
			 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children)
	    end;
	

	
	{tv_new_table, NewTabWinPid, Node, Name, Options, KindOfTableToCreate, Readable, true} ->
	    case create_table(KindOfTableToCreate, Node, Node =:= node(), Name, Options, 
			      NewTabWinPid) of
		error ->
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize, 
			 Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		TabId ->
		    case start_tv_browser(TabId,Node,Name,KindOfTableToCreate,Readable,Children) of
			Children ->
			    {FinalMarkedCell, NewTables, NewGridLines} = 
				case KindOfTable of
				    mnesia ->
					{MarkedCell, Tables, GridLines};
				    ets ->
					refresh_window(MarkedCell,Tables,KindOfTable,
						       CurrNode,GridLines,UnreadHidden,
						       SysTabHidden,SortKey, Children)
				end,
			    loop(KindOfTable,CurrNode,FinalMarkedCell,NewGridLines,WinSize,
				 NewTables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
			NewChildren ->
			    case KindOfTable of
				mnesia ->
				    done;
				ets ->
				    self() ! {gs, tv_main, click, update, []}
			    end,
			    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,
				 Shortcuts,UnreadHidden,SysTabHidden,SortKey,NewChildren)
		    end
	    end;
	

	
	{'EXIT', Pid, _Reason} ->
	    case lists:keysearch(Pid, 1, Children) of
		false ->
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize, 
			 Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
		{value, {Pid,Prog,_Data}} ->
		    NewChildren = 
			case Prog of
			    tv_nodewin ->
				lists:keydelete(Pid, 1, Children) ++ start_tv_nodewin(CurrNode);
			    _Other ->
				lists:keydelete(Pid, 1, Children)
			end,
		    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize, 
			 Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,NewChildren)
	    end;


	_Other ->
	    loop(KindOfTable, CurrNode, MarkedCell, GridLines, WinSize, Tables, Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,Children)
    end.


flush_msgs(Msg0 = {gs, Win, Op, _, _}) ->
    receive Msg = {gs, Win,Op,_,_} ->
	    flush_msgs(Msg)
    after 100 ->
	    Msg0
    end.

handle_keypress(open_table,KindOfTable,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    NewChildren = 
	case MarkedCell of
	    {undefined,_,_} ->
		case get(error_msg_mode) of
		    normal ->
			gs:config(win, [beep]),
			tv_utils:notify(win, "TV Notification", "No table selected!");
		    haiku ->
			Msg = ["Rather than a beep",
			       "Or a rude error message",
			       "These words: make a choice."],
			tv_utils:notify(win, "TV Notification", Msg)
		end,
		Children;
	    _OtherCell ->
		{Table, Name, Readable} = get_table_id(KindOfTable, element(3, MarkedCell), 
						       Tables),
		start_tv_browser(Table, CurrNode, Name, KindOfTable, Readable, Children)
	end,
    case NewChildren of
	Children ->
	    {NewMarkedCell, NewTables, NewGridLines} = 
		refresh_window(MarkedCell,Tables,KindOfTable,CurrNode,GridLines,UnreadHidden,
			       SysTabHidden, SortKey, Children),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,
		 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
	_Other ->
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,NewChildren)
    end;


handle_keypress(update,KindOfTable,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    gs:config(win, [{cursor,busy}]),
    NewMarkedCell = unmark_cell(MarkedCell, Tables),
    {NewTabs, NewGrLines} = 
	update_grid(KindOfTable,CurrNode,GridLines,UnreadHidden,SysTabHidden,SortKey),
    update_tv_info(Children),
    gs:config(win, [{cursor,arrow}]),
    loop(KindOfTable,CurrNode,NewMarkedCell,NewGrLines,WinSize,NewTabs,Shortcuts,
	 UnreadHidden,SysTabHidden,SortKey,Children);


handle_keypress(show_mnesia,ets,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    gs:config(win, [{cursor,busy}]),
    NewMarkedCell = unmark_cell(MarkedCell, Tables),
    gs:config(label2, [{fg, ?DISABLED_COLOR}]),
    gs:config(label3, [{fg, ?DISABLED_COLOR}]),
    gs:config(label4, [{fg, ?DISABLED_COLOR}]),
    gs:config(show_unreadable, [{label, {text, " Unreadable Tables "}},
				{data, show_unreadable}]),
    %% gs:config(show_unreadable, [{enable, false},
    %% 				   {select, false}]),
    gs:config(sort_table_id, [{enable, false}]),
    NewSortKey = 
	case SortKey of
	    ?ID_ELEM ->
		gs:config(sort_table_name, [{select,true}]),
		?NAME_ELEM;
	    _Other ->
		SortKey
	end,
    {NewTables, NewGridLines} = 
	update_grid(mnesia,CurrNode,GridLines,UnreadHidden,SysTabHidden,NewSortKey),
    gs:config(win, [{cursor,arrow}]),
    loop(mnesia,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
	 UnreadHidden,SysTabHidden,NewSortKey,Children);



handle_keypress(show_ets,mnesia,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    gs:config(win, [{cursor,busy}]),
    NewMarkedCell = unmark_cell(MarkedCell, Tables),
    gs:config(label2, [{fg, ?NORMAL_FG_COLOR}]),
    gs:config(label3, [{fg, ?NORMAL_FG_COLOR}]),
    gs:config(label4, [{fg, ?NORMAL_FG_COLOR}]),
    {NewTables, NewGridLines} = 
	update_grid(ets,CurrNode,GridLines,UnreadHidden,SysTabHidden,SortKey),
    %% gs:config(show_unreadable, [{enable, true},
    %%				   {select, not(UnreadHidden)}]),
    gs:config(sort_table_id, [{enable, true}]),
    gs:config(win, [{cursor,arrow}]),
    loop(ets,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,Shortcuts,
	 UnreadHidden,SysTabHidden,SortKey,Children);


handle_keypress(trace_process,KindOfTable,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    NewChildren = 
	case MarkedCell of
	    {_Id, ?PID_COL, Row} ->
		OwnerPid = element(?PID_ELEM, lists:nth(Row, Tables)),
		start_pman(OwnerPid, Children);
	    {_Id, ?PROCNAME_COL, Row} ->
		OwnerPid = element(?PID_ELEM, lists:nth(Row, Tables)),
		start_pman(OwnerPid, Children);
	    _Other ->
		Children
	end,
    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
	 UnreadHidden,SysTabHidden,SortKey, NewChildren);


handle_keypress(select_node,KindOfTable,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    show_tv_nodewin(Children),
    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
	 UnreadHidden,SysTabHidden,SortKey,Children);


handle_keypress(show_info,KindOfTable,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    NewChildren = 
	case MarkedCell of
	    {_Id, ?NAME_COL, Row} ->
		{Table, _Name, _Readable} = get_table_id(KindOfTable, Row, Tables),
		start_tv_info(Table, CurrNode, CurrNode =:= node(), KindOfTable, Children);
	    {_Id, ?ID_COL, Row} ->
		{Table, _Name, _Readable} = get_table_id(KindOfTable, Row, Tables),
		start_tv_info(Table, CurrNode, CurrNode =:= node(), KindOfTable, Children);
	    {_Id, ?INFO_COL, Row} ->
		{Table, _Name, _Readable} = get_table_id(KindOfTable, Row, Tables),
		start_tv_info(Table, CurrNode, CurrNode =:= node(), KindOfTable, Children);
	    _OtherCell ->
		Children
	end,
    case NewChildren of
	Children ->
	    {NewMarkedCell, NewTables, NewGridLines} = 
		refresh_window(MarkedCell,Tables,KindOfTable,CurrNode,GridLines,UnreadHidden,
			       SysTabHidden, SortKey, Children),
	    loop(KindOfTable,CurrNode,NewMarkedCell,NewGridLines,WinSize,NewTables,
		 Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children);
	_Other ->
	    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
		 UnreadHidden,SysTabHidden,SortKey,NewChildren)
    end;
    

handle_keypress(help_button,KindOfTable,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    HelpFile = filename:join([code:lib_dir(tv), "doc", "html", "index.html"]),
    tool_utils:open_help(win, HelpFile),
    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
	 UnreadHidden,SysTabHidden,SortKey,Children);

handle_keypress(exit_button,_KindOfTable,_CurrNode,_MarkedCell,_GridLines,
		_WinSize,_Tables,_Shortcuts,_UnreadHidden,_SysTabHidden,_SortKey,Children) ->
    lists:foreach(
      fun({Pid,pman,_OP}) ->
	      exit(Pid,kill);
	 (_) ->
	      done
      end, 
      Children),
    exit(normal);


handle_keypress(_Any,KindOfTable,CurrNode,MarkedCell,GridLines,
		WinSize,Tables,Shortcuts,UnreadHidden,SysTabHidden,SortKey,Children) ->
    loop(KindOfTable,CurrNode,MarkedCell,GridLines,WinSize,Tables,Shortcuts,
	 UnreadHidden,SysTabHidden,SortKey,Children).




refresh_window(MarkedCell,Tables,KindOfTable,
	       CurrNode,GridLines,UnreadHidden,SysTabHidden, SortKey, Children) ->
    gs:config(win, [{cursor,busy}]),
    NewMarkedCell = unmark_cell(MarkedCell, Tables),
    {NewTables, NewGridLines} = 
	update_grid(KindOfTable,CurrNode,GridLines,UnreadHidden,SysTabHidden,
		    SortKey),
    update_tv_info(Children),
    gs:config(win, [{cursor,arrow}]),
    {NewMarkedCell, NewTables, NewGridLines}.
    




get_table_id(mnesia, Row, Tables) ->
    TabTuple = lists:nth(Row, Tables),
    Readable = element(?READABLE_ELEM, TabTuple),
    Id       = element(?NAME_ELEM, TabTuple),
    {Id, Id, Readable};
get_table_id(ets, Row, Tables) ->
    TabTuple = lists:nth(Row, Tables),
    Readable = element(?READABLE_ELEM, TabTuple),
    Name     = element(?NAME_ELEM, TabTuple),
    case element(?NAMED_TABLE_ELEM, TabTuple) of
	false ->
	    {element(?ID_ELEM, TabTuple), Name, Readable};
	_Other ->
	    {Name, Name, Readable}
    end.
	


replace_node_name('nonode@nohost', 'nonode@nohost') ->
       %% Still undistributed...
    false;
replace_node_name(_Node, _OldNode) when node() =:= 'nonode@nohost' ->
       %% No longer distributed, but previously was!
    true;
replace_node_name(_Node, 'nonode@nohost') ->
       %% The system has been distributed!
    true;
replace_node_name(_Node, _OldNode) ->
    false.



update_node_name(Children) when node() =:= 'nonode@nohost' ->
       %% We have been distributed, but no longer are!
       %% We change all node names stored to 'nonode@nohost'!
       %% This works because we *will* receive exit signals
       %% for those processes that have died on other nodes,
       %% whereupon these processes will be removed from the
       %% 'Children' list.
    lists:map(fun({Pid, Prog, {Table,_Node}}) ->
		      {Pid, Prog, {Table,'nonode@nohost'}};
		 (H) ->
		      H
	      end,
	      Children);
update_node_name(Children) ->
       %% We have become distributed!
       %% Change all occurrences of 'nonode@nohost'
       %% to the new current node name!
    HomeNode = node(),
    lists:map(fun({Pid, Prog, {Table,'nonode@nohost'}}) ->
		      {Pid, Prog, {Table,HomeNode}};
		 (H) ->
		      H
	      end,
	      Children).
    



show_tv_nodewin(Children) ->
    {value, {Pid,tv_nodewin,_Node}} = lists:keysearch(tv_nodewin, 2, Children),
    Pid ! show_window.



update_tv_info(Children) ->
    Sender = self(),
    lists:foreach(fun({Pid,tv_info,{_Table,_Node}}) ->
			  Pid ! #info_update_table_info{sender=Sender};
		     (_) ->
			  done
		  end,
		  Children).



update_tv_browser(Children) ->
    lists:foreach(fun({Pid,tv_browser,{_Table,_Node}}) ->
			  Pid ! check_node;
		     (_) ->
			  done
		  end,
		  Children).



get_tv_info_pid(TabId,Node,Children) ->
    TvInfoChildren = [X || X <- Children, element(2,X) =:= tv_info],
    case lists:keysearch({TabId,Node}, 3, TvInfoChildren) of
	{value, {Pid, tv_info, {_Table,Node}}} ->
	    Pid;
	_Other ->
	    undefined
    end.



start_tv_browser(Tab,Node,_Name,KindOfTable,false,Children) ->
    gs:config(win, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(win, "TV Notification", 
			    ["The selected table is unreadable!", 
			     "Only table information may be viewed!"]);
	haiku ->
	    Msg = ["Table protected.",
		   "The answers that you're seeking",
		   "will remain unknown."],
	    tv_utils:notify(win, "TV Notification", Msg)
    end,
    start_tv_info(Tab, Node, Node =:= node(), KindOfTable, Children);
start_tv_browser(Table,Node,Name,KindOfTable,_Readable,Children) ->
    TvBrowserChildren = [X || X <- Children, element(2,X) =:= tv_browser],
    case lists:keysearch({Table,Node}, 3, TvBrowserChildren) of
	{value, {BPid,tv_browser,{Table,Node}}} ->
	    BPid ! raise,
	    Children;
	_Other ->
	       %% Check that table still exists!
	    case table_still_there(KindOfTable, Node, Node =:= node(), Table, Name) of
		true ->
		    LocalNode = (Node =:= node()),
		    NewBPid = tv:start_browser(Node, LocalNode, Table, KindOfTable, Name, 
					       get(error_msg_mode)),
		    [{NewBPid, tv_browser, {Table,Node}} | Children];
		_TableDead ->
		    gs:config(win, [beep]),
		    case get(error_msg_mode) of
			normal ->
			    tv_utils:notify(win, "TV Notification", 
					    ["The table no longer exists!"]);
			haiku ->
			    Msg = ["A table that big?",
				   "It might be very useful.",
				   "But now it is gone."],
			    tv_utils:notify(win, "TV Notification", Msg)
		    end,
		    Children
	    end
    end.





table_still_there(ets, Node, LocalNode, Table, Name) ->
    case catch tv_ets_rpc:all(Node, LocalNode) of
	Tables when is_list(Tables) ->
	    case lists:member(Table, Tables) of   
		true ->
		    true;
		false ->   %% May be a named table...
		    lists:keymember(Name, 1, Tables)
	    end;
	Error ->
	    analyze_error(Error, Node, Table),
	    false
    end;
table_still_there(mnesia, Node, LocalNode, Table, Name) ->
    case catch tv_mnesia_rpc:system_info(Node, LocalNode, tables) of
	Tables when is_list(Tables) ->
	    lists:member(Name, Tables);
	Error ->
	    analyze_error(Error, Node, Table),
	    false
    end.

    




start_tv_info(Table, Node, LocalNode, KindOfTable, Children) ->
    TvInfoChildren = [X || X <- Children, element(2,X) =:= tv_info],
    case lists:keysearch({Table,Node}, 3, TvInfoChildren) of
	{value, {Pid,tv_info,{Table,Node}}} ->
	    Pid ! #info_raise_window{sender = self()},
	    Children;
	_Other ->
	       %% May have started a browser but no info window!
	       %% Info window may have been started from that browser, but
	       %% don't bother with checking *that*.
	    Pid = spawn_link(tv_info, info, [self(), Node, LocalNode, Table, KindOfTable, 
					     get(error_msg_mode)]),
	    [{Pid, tv_info, {Table,Node}} | Children]
    end.
    




start_tv_new_table(CurrNode, Children) ->
    TvNewTableChild = [X || X <- Children, element(2,X) =:= tv_new_table],
    case TvNewTableChild of
	[{Pid,tv_new_table,undefined}] ->
	    Pid ! raise,
	    Children;
	[] ->
	    Pid = tv_new_table:start(CurrNode, get(error_msg_mode)),
	    [{Pid, tv_new_table, undefined} | Children]
    end.
    



create_table(mnesia, _Node, _LocalNode, _TabName, _Options, _NewTabWinPid) ->
    error;
create_table(ets, Node, LocalNode, TabName, Options, NewTabWinPid) ->
    case tv_table_owner:create(ets, Node, LocalNode, TabName, Options) of
	{ok, TabId} ->
	    NewTabWinPid ! ok,
	    TabId;
	error ->
	    NewTabWinPid ! error,
	    error
    end.




start_pman(OwnerPid, Children) ->
    Pid = pman_shell:start(OwnerPid),
    [{Pid,pman,OwnerPid} | Children].




update_grid(TableType, CurrNode, GridLines, UnreadHidden, SysTabHidden,SortKey) ->
    NewTables = get_tables(CurrNode, TableType, UnreadHidden, SysTabHidden,SortKey),
    TabStr = case TableType of
		 mnesia ->
		     "Mnesia ";
		 ets ->
		     "ETS "
	     end,
    NodeStr = atom_to_list(CurrNode),
    gs:config(win, [{title, "[TV]   " ++ TabStr ++ "tables on " ++ NodeStr}]),
    gs:config(grid, [{rows, {1, get_nof_rows(length(NewTables), gs:read(grid,height))}}]),
    NewGridLines = update_gridlines(NewTables, GridLines, 1),
    {NewTables, NewGridLines}.
    
    

unmark_cell({undefined, AnyCol, AnyRow}, _Tables) ->
    {undefined, AnyCol, AnyRow};
unmark_cell({Id, Col, Row}, Tables) ->
    disable_menus(),
    TabTuple = lists:nth(Row, Tables),
    ReadableTable = element(?READABLE_ELEM, TabTuple),
    NamedTable = element(?NAMED_TABLE_ELEM, TabTuple),
    BgColor = 
	case ReadableTable of
	    false ->
		?UNREADABLE_BG_COLOR;
	    _Other1 ->
		?READABLE_BG_COLOR
	end,
    
    FgColor = 
	case NamedTable of
	    false when Col =:= ?NAME_COL ->
		?UNNAMED_FG_COLOR;
	    _Other2 ->
		?NORMAL_FG_COLOR
	end,

    gs:config(Id, [{bg, {Col, BgColor}},
		   {fg, {Col, FgColor}}]),
    {undefined, undefined, undefined}.




mark_cell({Id,Col,Row}, {Id,Col,Row}, _Readable) ->
    {undefined, undefined, undefined};
mark_cell({Id,Col,Row}, _Any, Readable) ->
    case lists:member(Col, ?POSSIBLE_MARK_COLS) of
	true ->
	    enable_menus(Col, Readable),
	    gs:config(Id, [{bg, {Col, ?GRID_MARK_COLOR}},
			   {fg, {Col, ?NORMAL_FG_COLOR}}]),
	    {Id, Col,Row};
	false ->
	    {undefined, undefined, undefined}
    end.


disable_menus() ->
    disable_open_menu(),
    disable_trace_menu(),
    disable_info_menu().


enable_menus(?ID_COL, true) ->
    enable_open_menu(),
    enable_info_menu();
enable_menus(?ID_COL, {notext}) ->
    enable_open_menu(),
    enable_info_menu();
enable_menus(?ID_COL, false) ->
    enable_info_menu();
enable_menus(?NAME_COL, true) ->
    enable_open_menu(),
    enable_info_menu();
enable_menus(?NAME_COL, {notext}) ->
    enable_open_menu(),
    enable_info_menu();
enable_menus(?NAME_COL, false) ->
    enable_info_menu();
enable_menus(?PID_COL, _Any) ->
    enable_trace_menu();
enable_menus(?PROCNAME_COL, _Any) ->
    enable_trace_menu();
enable_menus(?INFO_COL, _Any) ->
    enable_info_menu();
enable_menus(_Col, _Any) ->
    done.
    


resize_window(Width, Height, NofElems) ->
    WinWidth  = lists:max([Width, ?MIN_WIN_WIDTH]),
    WinHeight = lists:max([Height, ?MIN_WIN_HEIGHT]),
    gs:config(win, [{width, WinWidth},
		    {height, WinHeight}
		   ]),
    {BgWidth, BgHeight, FgWidth, FgHeight} = get_frame_coords(WinWidth, WinHeight),
    {GridWidth, GridHeight} = get_grid_coords(FgWidth, FgHeight),
    ColWidths = get_col_widths(?COL_WIDTHS, GridWidth),
    resize_header_labels(ColWidths, 
			 [label1,label2,label3,label4,label5], 
			 ?GRID_XPOS),
    gs:config(bgframe, [{width, BgWidth},
			{height, BgHeight}
		       ]),
    gs:config(fgframe, [{width, FgWidth},
			{height, FgHeight}
		       ]),
    gs:config(grid, [{width, GridWidth},
		     {height, GridHeight},
		     {columnwidths, ColWidths},
		     {rows, {1, get_nof_rows(NofElems, GridHeight)}}
		    ]),
    {WinWidth, WinHeight}.




create_window(Tables) ->
    gs:window(win, gs:start(), [{width, ?WIN_WIDTH},
				{height, ?WIN_HEIGHT},
				{bg, ?DEFAULT_BG_COLOR},
				{title, "[TV]   ETS tables on " ++ 
				atom_to_list(node())},
				{destroy, true},
				{configure, true},
				{keypress, true}
			       ]),

    ShortcutList = create_menus(),

    disable_menus(),

    {BgFrameWidth, BgFrameHeight, FgFrameWidth, FgFrameHeight} = 
	get_frame_coords(?WIN_WIDTH, ?WIN_HEIGHT),

    {GridWidth, GridHeight} = get_grid_coords(FgFrameWidth, FgFrameHeight),

    ColWidths = get_col_widths(?COL_WIDTHS, GridWidth),

    gs:frame(bgframe, win, [{width, BgFrameWidth},
			    {height, BgFrameHeight},
			    {x, ?GRID_XPOS},
			    {y, ?GRID_YPOS},
			    {bg, {0,0,0}}
			   ]),
    gs:frame(fgframe, bgframe, [{width, FgFrameWidth},
				{height, FgFrameHeight},
				{x, 0},
				{y, 1},
				{bg, ?DEFAULT_BG_COLOR}
			       ]),


    create_header_labels(ColWidths, ?HEADER_LABELS),
    gs:grid(grid, fgframe, [{width, GridWidth},
			    {height, GridHeight},
			    {x, 0},
			    {y, -1},
			    {hscroll,bottom},
			    {vscroll,right},
			    {rows, {1, get_nof_rows(length(Tables), GridHeight)}},
			    {columnwidths, ColWidths},
			    {fg, ?NORMAL_FG_COLOR},
			    {bg, {255,255,255}},
			    {font, ?FONT}
			   ]),
    GridLines = update_gridlines(Tables, [], 1),
    {{undefined,undefined,undefined}, GridLines, {?WIN_WIDTH,?WIN_HEIGHT}, ShortcutList}.
    
			 


get_frame_coords(WinWidth, WinHeight) ->
    BgWidth  = WinWidth - 2 * ?GRID_XPOS,
    BgHeight = WinHeight - ?GRID_YPOS - ?GRID_XPOS,
    FgWidth  = BgWidth,
    FgHeight = BgHeight - 1,
    {BgWidth, BgHeight, FgWidth, FgHeight}.




get_grid_coords(ParentWidth, ParentHeight) ->
    {ParentWidth, ParentHeight + 1}.



get_col_widths(Cols, GridWidth) ->
    SbWidth = 25, %% OK, OK, don't bother about it, this constant makes it work...  :-/
    FixColWidthSum = lists:sum(lists:map(fun(H) ->
						 lists:nth(H, Cols)
					 end,
					 ?FIX_WIDTH_COLS)),
    AvailableWidth = GridWidth - FixColWidthSum - SbWidth,
    OriginalWidth  = ?WIN_WIDTH - 2 * ?GRID_XPOS - FixColWidthSum - SbWidth,
    get_col_widths(1, Cols, AvailableWidth, OriginalWidth).
    


get_col_widths(N, [H | T], AvailWidth, OrigWidth) ->
    NewColWidth = 
	case lists:member(N, ?FIX_WIDTH_COLS) of
	    true ->
		H;
	    _Other ->
		round(H * (AvailWidth / OrigWidth) + 0.1)
	end,
    [NewColWidth | get_col_widths(N + 1, T, AvailWidth, OrigWidth)];
get_col_widths(_N, [], _AvailWidth, _OrigWidth) ->
    [].



create_header_labels(ColWidths, Text) ->
    create_header_labels(ColWidths, Text, 1, ?GRID_XPOS).



create_header_labels([W | T], [{Name, Text} | TextT], N, Xpos) ->
    Ypos = ?GRID_YPOS - 20,
    gs:label(Name, win, [{width, W + 1 - 3},
			 {height, 20},
			 {x, Xpos + 1 + 3},
			 {y, Ypos},
			 {bg, ?DEFAULT_BG_COLOR},
			 {fg, ?NORMAL_FG_COLOR},
			 {font, ?HEADER_FONT},
			 {align, w},
			 {label, {text, Text}}
			]),
    create_header_labels(T, TextT, N + 1, Xpos + 1 + W);
create_header_labels([], [], _N, _Xpos) ->
    done.



resize_header_labels([W | T], [Name | NT], Xpos) ->
    gs:config(Name, [{width, W + 1 - 3},
		     {x, Xpos + 1 + 3}
		    ]),
    resize_header_labels(T, NT, Xpos + 1 + W);
resize_header_labels([], [], _Xpos) ->
    done.



disable_open_menu() ->
    gs:config(open_table, [{enable,false}]).


disable_info_menu() ->
    gs:config(show_info, [{enable,false}]).

disable_trace_menu() ->
    gs:config(trace_process, [{enable,false}]).


enable_open_menu() ->
    gs:config(open_table, [{enable,true}]).


enable_info_menu() ->
    gs:config(show_info, [{enable,true}]).


enable_trace_menu() ->
    gs:config(trace_process, [{enable,true}]).


create_menus() ->
    gs:menubar(menubar, win, [{bg, ?DEFAULT_BG_COLOR}]),

    HelpButt = gs:menubutton(menubar, [{bg, ?DEFAULT_BG_COLOR},
				       {fg, ?FIREBRICK},  % firebrick
				       {label, {text, " Help "}},
				       {underline, 1},
				       {side, right}
				      ]),
    FileButt = gs:menubutton(menubar, [{bg, ?DEFAULT_BG_COLOR},
				       {fg, ?FIREBRICK},  % firebrick
				       {label, {text, " File "}},
				       {underline, 1},
				       {side, left}
				      ]),
    ViewButt = gs:menubutton(menubar, [{bg, ?DEFAULT_BG_COLOR},
				       {fg, ?FIREBRICK},  % firebrick
				       {label, {text, " View "}}, 
				       {underline, 1},
				       {side, left}
				      ]),
    OptionsButt = gs:menubutton(menubar, [{bg, ?DEFAULT_BG_COLOR},
					  {fg, ?FIREBRICK},  % firebrick
					  {label, {text, " Options "}},
					  {underline, 1},
					  {side, left}
					 ]),

    HelpMenu = gs:menu(HelpButt, [{bg, ?DEFAULT_BG_COLOR},
				  {fg, ?FIREBRICK},
				  {disabledfg,?DISABLED_COLOR}
				 ]), 
    FileMenu = gs:menu(FileButt, [{bg, ?DEFAULT_BG_COLOR},
				  {fg, ?FIREBRICK},
				  {disabledfg,?DISABLED_COLOR}
				 ]), 

    OptionsMenu = gs:menu(OptionsButt, [{bg, ?DEFAULT_BG_COLOR},
					{fg, ?FIREBRICK},
					{disabledfg,?DISABLED_COLOR}
				       ]), 

    ViewMenu = gs:menu(ViewButt, [{bg, ?DEFAULT_BG_COLOR},
				  {fg, ?FIREBRICK},
				  {disabledfg,?DISABLED_COLOR}
				 ]),

    ShortCutList = 
	create_menulist([{" Help ",normal,help_button,1,h},
			 separator,
			 {" OTP Documentation ",normal,otp_help_button,1,no_char}], HelpMenu) ++
	create_menulist([{" Open Table ",normal,open_table,1,o}, 
			 {" New Table... ",normal,new_table,1,no_char},
			 {" Table Info ",normal,show_info,7,i},
			 separator, 
			 {" Nodes... ",normal,select_node,1,n},
			 separator,
			 {" Trace Process ",normal,trace_process,1,t},
			 separator,
			 {" Exit ",normal, exit_button,2,x}], FileMenu) ++
	[{c,exit_button}, {'C',exit_button}] ++
	create_menulist([{" Refresh ",normal,update,1,r},
			 separator,
			 {" Unreadable Tables ",check,show_unreadable,1,no_char},
			 separator,
			 {" System Tables ",check,show_system,1,no_char},
			 separator,
			 {" Sort by Name ",radio,sort_table_name,9,no_char},
			 {" Sort by Id ",radio,sort_table_id,9,no_char},
			 {" Sort by Owner PID ",radio,sort_owner_pid,15,no_char},
			 {" Sort by Owner Name ",radio,sort_owner_name,9,no_char},
			 separator,
			 {" Error Messages in Haiku ",check,show_haiku,1,no_char}
			], 
			OptionsMenu) ++
	create_menulist([{" ETS Tables ",radio,show_ets,1,e},
			 {" Mnesia Tables ",radio,show_mnesia,1,m}], ViewMenu),
    gs:config(show_unreadable, [{select,false}]),
    gs:config(show_system, [{select,false}]),
    gs:config(show_haiku, [{select,false}]),
       %% Due to a bug (or some other reason), only one of the radiobuttons belonging
       %% to a specified group can be selected, even if different processes have created
       %% the radiobuttons! This means that, if we have started more than one tv_main 
       %% process, selecting one radiobutton will affect the radiobuttons in the other 
       %% tv_main process(es)!!! Since this is a highly undesirable bahaviour, we have to 
       %% create unique group names (i.e., atoms). 
       %% (We need to group the radiobuttons, since otherwise all created by one process
       %% belongs to the same group, which also is undesirable...)
    SelfStr     = pid_to_list(self()),
    SortGroup   = list_to_atom("sorting" ++ SelfStr),
    TypeGroup   = list_to_atom("table_type" ++ SelfStr),
    gs:config(sort_table_name, [{group,SortGroup},{select,true}]),
    gs:config(sort_table_id, [{group,SortGroup}]),
    gs:config(sort_owner_pid, [{group,SortGroup}]),
    gs:config(sort_owner_name, [{group,SortGroup}]),
    gs:config(show_ets, [{group,TypeGroup}, {select,true}]),
    gs:config(show_mnesia, [{group,TypeGroup}]),
    ShortCutList.





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
    gs:menuitem(Data, Menu, [{bg, ?DEFAULT_BG_COLOR},
			     {fg, ?FIREBRICK},
			     {itemtype, Type},
			     {label, {text, FinalText}},
			     {underline, AccCharPos},
			     {data, Data}
			    ]),
    [{ShortcutChar, Data}, {ShortcutCapitalChar, Data} | create_menulist(Rest, Menu, MaxLength)];
create_menulist([separator | Rest], Menu, MaxLength) ->
    gs:menuitem(Menu, [{itemtype, separator}]),
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






get_nof_rows(NofElems, GridHeight) ->
    lists:max([NofElems, round((GridHeight - 20) / 21) + 1]).



config_gridline(LineId, TabTuple) ->
    Readable   = element(?READABLE_ELEM, TabTuple),
    NamedTable = element(?NAMED_TABLE_ELEM, TabTuple),
    {FgColor, BgColor} = 
	case Readable of
	    true ->
		{?NORMAL_FG_COLOR, ?READABLE_BG_COLOR};
	    false ->
		{?UNREADABLE_FG_COLOR, ?UNREADABLE_BG_COLOR};
	    {notext} ->
		{?NORMAL_FG_COLOR, ?READABLE_BG_COLOR}
	end,
    
    NameFgColor = 
	case NamedTable of
	    false ->
		?UNNAMED_FG_COLOR;
	    _Other ->
		?NORMAL_FG_COLOR
	end,
    
    gs:config(LineId, [{bg, BgColor},
		       {fg, FgColor},
		       {fg, {?NAME_COL, NameFgColor}},
		       {click, true},
		       {doubleclick, true},
		       {data, {grid,Readable}} |

		       lists:map(
			 fun({Elem,Col}) ->
				 case element(Elem, TabTuple) of
				     {notext} ->
					 {text, {Col, ""}};
				     Other when Elem =:= ?NAME_ELEM ->
					 case NamedTable of
					     false ->
						 {text, {Col, " " ++ 
							 lists:flatten(
							   io_lib:write(
							     Other)) ++ " "}};
					     _AnyOther ->
						 {text, {Col, " " ++ lists:flatten(
								       io_lib:write(
									 Other))}}
					 end;
				     Other ->
					 {text, {Col, " " ++ lists:flatten(
							       io_lib:write(
								 Other))}}
				 end
			 end, 
			 [{?NAME_ELEM,     ?NAME_COL}, 
			  {?ID_ELEM,       ?ID_COL}, 
			  {?PID_ELEM,      ?PID_COL}, 
			  {?PROCNAME_ELEM, ?PROCNAME_COL}, 
			  {?INFO_ELEM,     ?INFO_COL}]
			)
		      ]).





update_gridlines([TabTuple | TT], [LineId | GT], CurrRow) ->
    config_gridline(LineId, TabTuple),
    [LineId | update_gridlines(TT, GT, CurrRow + 1)];
update_gridlines([TabTuple | TT], [], CurrRow) ->
    LineId = gs:gridline(grid, [{row, CurrRow}]),
    config_gridline(LineId, TabTuple),
    [LineId | update_gridlines(TT, [], CurrRow + 1)];
update_gridlines([], [LineId | GT], _CurrRow) ->
    gs:destroy(LineId),
    update_gridlines([], GT, _CurrRow);
update_gridlines([], [], _CurrRow) ->
    [].







    
			   
